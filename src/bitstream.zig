const std = @import("std");

const State = struct {
    bit_offset: u4 = 8,
    current_byte: isize = -1,
    consumed_bytes: u64 = 0,
};

fn reverseBits(v: anytype, width: u8) @TypeOf(v) {
    comptime {
        if (@TypeOf(v) == u1) {
            return v;
        }
    }

    if (width <= 1) {
        return v;
    }

    if (@typeInfo(@TypeOf(v)).Int.bits < width) {
        @panic("reverseBits: value too small");
    }

    var result: @TypeOf(v) = 0;

    var n = v;
    var i: usize = 0;
    while (i < width) : (i += 1) {
        result = (result << 1) | (n & 1);
        n >>= 1;
    }

    return result;
}

const ReadBitsError = error{
    UnexpectedEndOfStream,
};

pub fn Fixed(comptime size: comptime_int) type {
    return struct {
        state: State = .{},

        buf: [size]u8 = undefined,
        len: usize = 0,

        const Self = @This();

        pub fn initWithData(data: []const u8) !Self {
            var bs = Self{};
            if (data.len > size) {
                return error.TooMuchInitialData;
            }

            if (data.len > 0) {
                std.mem.copy(u8, bs.buf[0..data.len], data);
                bs.len = data.len;
            }

            return bs;
        }

        pub fn readBits(self: *Self, comptime T: type, bits: u8, initialValue: T) !T {
            if (@typeInfo(T) != .Int) {
                @compileError("T must be an integer type");
            }

            var saved_state = self.state;
            errdefer {
                self.state = saved_state;
            }

            var n = bits;
            var result = initialValue;

            while (n > 0) : (n -= 1) {
                if (self.state.bit_offset == 8) {
                    if (self.state.current_byte + 1 == self.len) {
                        return error.UnexpectedEndOfStream;
                    }

                    // load next byte
                    self.state.current_byte += 1;
                    self.state.bit_offset = 0;
                    self.state.consumed_bytes += 1;
                }

                if (@typeInfo(T).Int.bits > 1) {
                    result <<= 1;
                }

                result |= @intCast(T, (self.buf[@intCast(usize, self.state.current_byte)] >> @intCast(u3, self.state.bit_offset)) & 1);
                self.state.bit_offset += 1;
            }

            return result;
        }

        pub fn write(self: *Self, buf: []const u8) usize {
            const aligned = self.state.bit_offset == 8;
            if ((self.state.current_byte > 1 or (self.state.current_byte > 0 and aligned))) {
                // copy remaining bytes to the beginning of the buffer
                const offset = @boolToInt(aligned);
                const head = @intCast(usize, self.state.current_byte + offset);

                if (head <= self.len) {
                    std.mem.copy(u8, &self.buf, self.buf[head..]);
                    self.len -= head;
                    self.state.current_byte -= @intCast(isize, head);
                } else {
                    self.len = 0;
                    self.state.current_byte = -1;
                }
            }

            const n = @min(self.buf.len - self.len, buf.len);
            std.mem.copy(u8, self.buf[self.len..], buf[0..n]);
            self.len += buf.len;

            return n;
        }

        pub fn readNumber(self: *Self, comptime T: type) !T {
            return try self.readNumberWidth(T, @typeInfo(T).Int.bits);
        }

        pub fn readNumberWidth(self: *Self, comptime T: type, width: u8) !T {
            return reverseBits(try self.readBits(T, width, 0), width);
        }

        pub fn readBytes(self: *Self, buf: []u8) ![]u8 {
            if (buf.len == 0) {
                return buf[0..0];
            }

            const head = @intCast(usize, self.state.current_byte + 1);
            if (head >= self.len) {
                return error.UnexpectedEndOfStream;
            }

            if (self.state.bit_offset == 0) {
                unreachable;
            }

            const n = @min(buf.len, self.len - head);
            if (self.state.bit_offset == 8) {
                std.mem.copy(u8, buf, self.buf[head..head + n]);
                self.state.current_byte += @intCast(isize, n);
            } else {
                var i: usize = 0;
                while (i < n) : (i += 1) {
                    buf[i] = @intCast(u8, try self.readNumber(u8));
                }
            }

            return buf[0..n];
        }

        pub fn alignToByte(self: *Self) void {
            self.state.bit_offset = 8;
        }
    };
}

pub const BitStream = Fixed(1 << 16);

const test_payload = [_]u8{ 0b01110011, 0b01010101 };

test "read bits" {
    var bs = Fixed(2).initWithData(&test_payload) catch unreachable;

    try std.testing.expectEqual(bs.readBits(u3, 3, 0), 0b110);
    try std.testing.expectEqual(bs.readBits(u1, 1, 0), 0b0);
    try std.testing.expectEqual(bs.readBits(u2, 2, 0), 0b11);
    try std.testing.expectEqual(bs.readBits(u1, 1, 0), 0b1);
    try std.testing.expectEqual(bs.readBits(u9, 9, 0), 0b010101010);
    try std.testing.expectError(error.UnexpectedEndOfStream, bs.readBits(u1, 1, 0));
}

test "read number" {
    var bs = Fixed(2).initWithData(&test_payload) catch unreachable;

    try std.testing.expectEqual(bs.readNumber(u3), 0b011);
    try std.testing.expectEqual(bs.readNumber(u1), 0b0);
    try std.testing.expectEqual(bs.readNumber(u2), 0b11);
    try std.testing.expectEqual(bs.readNumber(u1), 0b1);
    try std.testing.expectEqual(bs.readNumber(u9), 0b010101010);
    try std.testing.expectError(error.UnexpectedEndOfStream, bs.readNumber(u1));
}

test "read aligned full" {
    var bs = Fixed(2).initWithData(&test_payload) catch unreachable;

    var buf: [2]u8 = undefined;
    try std.testing.expectEqualSlices(u8, try bs.readBytes(buf[0..]), &test_payload);
}

test "read aligned partial" {
    var bs = Fixed(2).initWithData(&test_payload) catch unreachable;

    var buf: [3]u8 = undefined;
    try std.testing.expectEqualSlices(u8, try bs.readBytes(buf[0..]), &test_payload);
}

test "read unaligned" {
    var bs = Fixed(2).initWithData(&test_payload) catch unreachable;

    try std.testing.expectEqual(bs.readBits(u1, 1, 0), 0b1);

    var buf: [2]u8 = undefined;
    try std.testing.expectEqualSlices(u8, try bs.readBytes(buf[0..]), &[_]u8{0b10111001});
}

test "read empty" {
    var bs = Fixed(2){};

    var buf: [2]u8 = undefined;
    try std.testing.expectError(error.UnexpectedEndOfStream, bs.readBytes(&buf));
}

test "read bytes" {
    var bs = Fixed(2).initWithData(&test_payload) catch unreachable;

    var buf: [1]u8 = undefined;
    try std.testing.expectEqual(test_payload[0], (try bs.readBytes(&buf))[0]);
    try std.testing.expectEqual(test_payload[1], (try bs.readBytes(&buf))[0]);
}
