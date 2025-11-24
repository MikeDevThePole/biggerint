const std = @import("std");

const allocator = std.heap.page_allocator;

const BigInt = struct {
    limbs: []u64,
    sign: bool, // Signifies an infinite amount of ones after the base value

    pub const zero = BigInt{ .limbs = &.{}, .sign = false };
    pub const minus_one = BigInt{ .limbs = &.{}, .sign = true };

    inline fn fillLimb(self: *const BigInt) u64 {
        return @as(u64, @intFromBool(!self.sign)) -% 1;
    }

    pub fn grow(self: *BigInt) !void {
        const new_limbs = try allocator.alloc(u64, self.limbs.len + 1);
        @memcpy(new_limbs[0..self.limbs.len], self.limbs);
        @memset(new_limbs[self.limbs.len..], self.fillLimb());
        allocator.free(self.limbs);
        self.limbs = new_limbs;
    }

    pub fn fromInteger(value: i65) !BigInt {
        const limbs = try allocator.alloc(u64, 1);
        limbs[0] = @truncate(@as(u65, @bitCast(value)));
        const sign = value < 0;
        return BigInt{ .limbs = limbs, .sign = sign };
    }

    pub fn intoInteger(self: *const BigInt) i65 {
        std.debug.assert(self.limbs.len <= 1);
        const value = if (self.limbs.len > 0) self.limbs[0] else self.fillLimb();
        const sign_bit = @as(u65, @intFromBool(self.sign)) << 64;
        return @bitCast(sign_bit | value);
    }

    pub fn shrink(self: *BigInt) !void {
        const removable = self.fillLimb();
        var i = self.limbs.len;
        while (i > 0) {
            i -= 1;
            if (self.limbs[i] != removable) {
                break;
            }
        }
        if (i != self.limbs.len) {
            const new_limbs = try allocator.alloc(u64, i);
            @memcpy(new_limbs, self.limbs[0..i]);
            allocator.free(self.limbs);
            self.limbs = new_limbs;
        }
    }

    pub fn deinit(self: *BigInt) void {
        allocator.free(self.limbs);
    }

    pub fn bitwiseNot(self: *BigInt) void {
        self.sign = !self.sign;
        for (self.limbs) |*limb| {
            limb.* = ~limb.*;
        }
    }

    pub fn inc(self: *BigInt) !void {
        for (self.limbs) |*limb| {
            const new_value, const overflow = @addWithOverflow(limb.*, 1);
            limb.* = new_value;
            if (overflow == 0) {
                if (self.sign) {
                    try self.shrink();
                }
                return;
            }
        }

        if (self.sign) {
            self.sign = false;
            return; // At this point the value should be zero
        }

        try self.grow();
        self.limbs[self.limbs.len - 1] = 1;
    }

    pub fn neg(self: *BigInt) !void {
        self.bitwiseNot();
        try self.inc();
    }

    pub fn add(self: *const BigInt, other: *const BigInt) !BigInt {
        var a = self;
        var b = other;
        if (a.limbs.len > b.limbs.len) {
            std.mem.swap(*const BigInt, &a, &b);
        }

        const new_limbs = try allocator.alloc(u64, b.limbs.len);

        var carry: u1 = 0;
        for (0..a.limbs.len) |i| {
            const sum1, const carry1 = @addWithOverflow(a.limbs[i], b.limbs[i]);
            const sum2, const carry2 = @addWithOverflow(sum1, carry);
            new_limbs[i] = sum2;
            carry = carry1 | carry2;
        }

        const a_fill = a.fillLimb();
        for (a.limbs.len..b.limbs.len) |i| {
            const sum1, const carry1 = @addWithOverflow(a_fill, b.limbs[i]);
            const sum2, const carry2 = @addWithOverflow(sum1, carry);
            new_limbs[i] = sum2;
            carry = carry1 | carry2;
        }

        var sum = BigInt{ .limbs = new_limbs, .sign = undefined };

        if (a.sign == b.sign) {
            sum.sign = a.sign;
            if (@intFromBool(a.sign) != carry) {
                try sum.grow();
                sum.limbs[sum.limbs.len - 1] = a_fill ^ 1;
            }
        } else {
            sum.sign = carry == 0;
            try sum.shrink();
        }

        return sum;
    }
};

pub fn main() !void {
    const a = try BigInt.fromInteger(std.math.minInt(i65));
    const b = try BigInt.fromInteger(-1);
    const sum = try BigInt.add(&a, &b);
    std.debug.print("{X}\n", .{@as([]const u8, @ptrCast(sum.limbs))});
    // var value = BigInt.zero;
    // for (0..128) |_| {
    //     try value.inc();
    //     std.debug.print("{any}\n", .{value});
    // }
}
