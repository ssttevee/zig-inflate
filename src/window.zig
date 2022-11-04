const std = @import("std");

pub fn Window(comptime size: comptime_int) type {
    return struct {
        buf: [size * 2]u8 = undefined,
        end: usize = 0,

        const Self = @This();

        pub fn initWithData(buf: []const u8) Self {
            var w = Self{};
            _ = w.write(buf);
            return w;
        }

        pub fn read(self: Self, distance: usize, length: usize, buf: []u8, offset: usize) ![]u8 {
            if (distance == 0) {
                return error.InvalidDistance;
            }

            if (offset > length) {
                return error.InvalidOffset;
            }

            if (distance > self.end or distance > size) {
                return error.OutOfRange;
            }

            if (length == 0 or buf.len == 0 or offset == buf.len) {
                return buf[0..0];
            }

            const head = self.end - distance;
            if (distance >= length) {
                std.mem.copy(u8, buf, self.buf[head + offset..head + @min(length, offset + buf.len)]);
            } else {
                const seq = self.buf[head..self.end];
                const iters = (length - offset) / distance;

                var i: usize = 0;
                while (i < iters) : (i += 1) {
                    const pos = i * distance + offset;
                    std.mem.copy(u8, buf[pos..], seq);
                }

                const rem = (length - offset) % distance;
                if (rem > 0) {
                    const pos = iters * distance + offset;
                    std.mem.copy(u8, buf[pos..], seq[0..rem]);
                }
            }

            return buf[0..length];
        }

        pub fn write(self: *Self, chunk: []const u8) []const u8 {
            if (chunk.len >= size) {
                std.mem.copy(u8, self.buf[0..], chunk[chunk.len - size..]);
                self.end = size;
            } else {
                const new_end = self.end + chunk.len;
                if (new_end > size) {
                    std.mem.copy(u8, self.buf[0..], self.buf[new_end - size..]);
                    self.end = size - chunk.len;
                }

                std.mem.copy(u8, self.buf[self.end..], chunk);
                self.end += chunk.len;
            }

            return chunk;
        }
    };
}

pub fn init(comptime size: comptime_int) Window(size) {
    return .{};
}

pub fn initWithData(comptime size: comptime_int, buf: []const u8) Window(size) {
    return Window(size).initWithData(buf);
}

test "write data > size" {
    _ = initWithData(0, "0");
    _ = initWithData(1, "01");
    _ = initWithData(1, "012");
}

test "read zero distance" {
    var window = init(0);
    try std.testing.expectError(error.InvalidDistance, window.read(0, 0, undefined, 0));
}

test "read from empty window" {
    var window = init(0);
    try std.testing.expectError(error.OutOfRange, window.read(1, 0, undefined, 0));
}

test "read full" {
    var window = initWithData(1, "0");

    var out = [_]u8{0} ** 1;
    try std.testing.expectEqualStrings("0", try window.read(1, 1, &out, 0));
}

test "read loop aligned" {
    var window = initWithData(1, "0");

    var out = [_]u8{0} ** 2;
    try std.testing.expectEqualStrings("00", try window.read(1, 2, &out, 0));
}

test "read loop unaligned" {
    var window = initWithData(2, "01");

    var out = [_]u8{0} ** 3;
    try std.testing.expectEqualStrings("010", try window.read(2, 3, &out, 0));
}
