const std = @import("std");

const allocator = std.heap.page_allocator;

const Limb = u64;

const BigInt = struct {
    limbs: []Limb,
    sign: bool, // Signifies an infinite amount of ones after the base value

    pub const zero = BigInt{ .limbs = &.{}, .sign = false };
    pub const minus_one = BigInt{ .limbs = &.{}, .sign = true };

    pub fn fromInteger(value: i65) !BigInt {
        const limbs = try allocator.alloc(Limb, 1);
        limbs[0] = @truncate(@as(u65, @bitCast(value)));
        const sign = value < 0;
        return BigInt{ .limbs = limbs, .sign = sign };
    }

    pub fn clone(self: *const BigInt) !BigInt {
        const new_limbs = try allocator.dupe(Limb, self.limbs);
        return BigInt{ .limbs = new_limbs, .sign = self.sign };
    }

    pub fn deinit(self: *BigInt) void {
        allocator.free(self.limbs);
        self.* = undefined;
    }

    pub fn isZero(self: *const BigInt) bool {
        return !self.sign and self.limbs.len == 0;
    }

    pub fn add(lhs: BigInt, rhs: BigInt) !BigInt {
        const min_limbs = @min(lhs.limbs.len, rhs.limbs.len);
        const max_limbs = @min(lhs.limbs.len, rhs.limbs.len);
        const new_limbs = try allocator.alloc(Limb, @max(lhs.limbs.len, rhs.limbs.len));

        var carry: u1 = 0;
        for (0..min_limbs) |i| {
            const sum1, const carry1 = @addWithOverflow(lhs.limbs[i], rhs.limbs[i]);
            const sum2, const carry2 = @addWithOverflow(sum1, carry);
            new_limbs[i] = sum2;
            carry = carry1 | carry2;
        }

        if (lhs.limbs.len != rhs.limbs.len) {
            const fill_value = if (lhs.limbs.len <= rhs.limbs.len) lhs.fillLimb() else rhs.fillLimb();
            const longer_arr = if (lhs.limbs.len <= rhs.limbs.len) lhs.limbs else rhs.limbs;
            for (min_limbs..max_limbs) |i| {
                const sum1, const carry1 = @addWithOverflow(fill_value, longer_arr[i]);
                const sum2, const carry2 = @addWithOverflow(sum1, carry);
                new_limbs[i] = sum2;
                carry = carry1 | carry2;
            }
        }

        var sum = BigInt{ .limbs = new_limbs, .sign = undefined };

        if (lhs.sign == rhs.sign) {
            sum.sign = lhs.sign;
            if (@intFromBool(lhs.sign) != carry) {
                try sum.grow();
                sum.limbs[sum.limbs.len - 1] = lhs.fillLimb() ^ 1;
            }
        } else {
            sum.sign = carry == 0;
            try sum.shrink();
        }

        return sum;
    }

    pub fn sub(lhs: BigInt, rhs: BigInt) !BigInt {
        const min_limbs = @min(lhs.limbs.len, rhs.limbs.len);
        const max_limbs = @min(lhs.limbs.len, rhs.limbs.len);
        const new_limbs = try allocator.alloc(Limb, max_limbs);

        var borrow: u1 = 0;

        for (0..min_limbs) |i| {
            const d1, const b1 = @subWithOverflow(lhs.limbs[i], rhs.limbs[i]);
            const d2, const b2 = @subWithOverflow(d1, borrow);
            new_limbs[i] = d2;
            borrow = b1 | b2;
        }

        if (lhs.limbs.len != rhs.limbs.len) {
            if (lhs.limbs.len <= rhs.limbs.len) {
                const lhs_fill = lhs.fillLimb();
                for (min_limbs..max_limbs) |i| {
                    const d1, const b1 = @subWithOverflow(lhs_fill, rhs.limbs[i]);
                    const d2, const b2 = @subWithOverflow(d1, borrow);
                    new_limbs[i] = d2;
                    borrow = b1 | b2;
                }
            } else {
                const rhs_fill = rhs.fillLimb();
                for (min_limbs..max_limbs) |i| {
                    const d1, const b1 = @subWithOverflow(lhs.limbs[i], rhs_fill);
                    const d2, const b2 = @subWithOverflow(d1, borrow);
                    new_limbs[i] = d2;
                    borrow = b1 | b2;
                }
            }
        }

        var result = BigInt{ .limbs = new_limbs, .sign = undefined };

        if (lhs.sign != rhs.sign) {
            result.sign = lhs.sign;
            if (@intFromBool(lhs.sign) == borrow) {
                try result.grow();
                result.limbs[result.limbs.len - 1] = lhs.fillLimb() ^ 1;
            }
        } else {
            result.sign = borrow != 0;
            try result.shrink();
        }

        return result;
    }

    inline fn fillLimb(self: *const BigInt) Limb {
        return @as(Limb, @intFromBool(!self.sign)) -% 1;
    }

    fn grow(self: *BigInt) !void {
        const new_limbs = try allocator.alloc(Limb, self.limbs.len + 1);
        @memcpy(new_limbs[0..self.limbs.len], self.limbs);
        allocator.free(self.limbs);
        self.limbs = new_limbs;
    }

    fn shrink(self: *BigInt) !void {
        const removable = self.fillLimb();
        var i = self.limbs.len;
        while (i > 0) : (i -= 1) {
            if (self.limbs[i - 1] != removable) {
                break;
            }
        }
        if (i != self.limbs.len) {
            const new_limbs = try allocator.alloc(Limb, i);
            @memcpy(new_limbs, self.limbs[0..i]);
            allocator.free(self.limbs);
            self.limbs = new_limbs;
        }
    }

    fn negMut(self: *BigInt) !void {
        self.sign = !self.sign;
        for (self.limbs) |*limb| {
            limb.* = ~limb.*;
        }

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
            return; // At this point the value should already have zero limbs
        }

        try self.grow();
        self.limbs[self.limbs.len - 1] = 1;
    }

    fn divModMut(self: *BigInt, divisor: Limb) !Limb {
        var remainder: Limb = 0;
        var i: usize = self.limbs.len;
        while (i > 0) {
            i -= 1;
            const value = (@as(u128, remainder) << 64) | self.limbs[i];
            self.limbs[i] = @intCast(value / divisor);
            remainder = @intCast(value % divisor);
        }
        try self.shrink();
        return remainder;
    }

    pub fn hex(self: BigInt) HexFormatter {
        return HexFormatter{ .value = self };
    }

    pub fn decimal(self: BigInt) DecimalFormatter {
        return DecimalFormatter{ .value = self };
    }
};

const HexFormatter = struct {
    value: BigInt,

    pub fn tryWrite(self: HexFormatter, writer: *std.io.Writer) !void {
        if (self.value.isZero()) {
            try writer.writeByte('0');
            return;
        }

        var copy = try self.value.clone();
        defer copy.deinit();

        if (copy.sign) {
            try copy.negMut();
            try writer.writeByte('-');
        }

        var i: usize = copy.limbs.len;
        var started = false;
        while (i > 0) {
            i -= 1;
            const limb = copy.limbs[i];
            var s: usize = 64;
            while (s > 0) {
                s -= 4;
                const digit: u8 = @intCast((limb >> @intCast(s)) & 0xF);
                if (!started) {
                    if (digit == 0) continue; // skip leading zeros
                    started = true;
                }
                try writer.writeByte("0123456789ABCDEF"[digit]);
            }
        }
    }

    pub fn format(self: HexFormatter, writer: *std.io.Writer) std.io.Writer.Error!void {
        return self.tryWrite(writer) catch error.WriteFailed;
    }
};

const DecimalFormatter = struct {
    value: BigInt,

    pub fn tryWrite(self: DecimalFormatter, writer: *std.Io.Writer) !void {
        if (self.value.isZero()) {
            try writer.writeByte('0');
            return;
        }

        var copy = try self.value.clone();
        defer copy.deinit();

        if (copy.sign) {
            try copy.negMut();
            try writer.writeByte('-');
        }

        var digits: std.ArrayList(u8) = .empty;
        defer digits.deinit(allocator);

        while (!copy.isZero()) {
            const rem = try copy.divModMut(10);
            try digits.append(allocator, @intCast(rem));
        }

        var i = digits.items.len;
        while (i > 0) {
            i -= 1;
            try writer.writeByte('0' + digits.items[i]);
        }
    }

    pub fn format(self: DecimalFormatter, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        return self.tryWrite(writer) catch error.WriteFailed;
    }
};

pub fn main() !void {
    const a = try BigInt.fromInteger(1);
    const b = try BigInt.fromInteger(234);
    const diff = try a.sub(b);
    std.debug.print("{f}\n", .{diff.decimal()});
    std.debug.print("Must be: {d}\n", .{1 - 234});
}
