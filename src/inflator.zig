const std = @import("std");

const BitStream = @import("./bitstream.zig").BitStream;
const Coding = @import("./coding.zig");
const Window = @import("./window.zig").Window;

const FixedBlockState = struct {
    coding: Coding,
    length: ?u9 = null,
    distance: ?u16 = null,
    pos: usize = 0,
};

const State = union (enum) {
    end_of_block: struct {},
    store_block: struct {
        len: ?u16 = null,
        pos: usize = 0,
    },
    fixed_block: FixedBlockState,
    dynamic_block: struct {},
};

const Inflator = struct {
    bs: BitStream = .{},
    bfinal: bool = false,
    closed: bool = false,

    state: State = .{ .end_of_block = .{} },
    window: Window(1 << 15) = .{},
    buf: [Coding.min_buf_size]u8 align(Coding.memory_alignment) = undefined,

    fn nextInternal(self: *Inflator, buf: []u8, pos: *usize) !?[]const u8 {
        outer: while (pos.* < buf.len) {
            switch (self.state) {
                .end_of_block => {
                    if (self.bfinal) {
                        if (pos.* > 0) {
                            break :outer;
                        }

                        return null;
                    }

                    self.bfinal = (try self.bs.readNumber(u1)) == 1;

                    switch (try self.bs.readNumber(u2)) {
                        0 => {
                            self.bs.alignToByte();
                            self.state = .{ .store_block = .{} };
                        },
                        1 => {
                            self.state = .{
                                .fixed_block = .{
                                    .coding = Coding.fixed,
                                },
                            };
                        },
                        2 => {
                            self.state = .{ .dynamic_block = .{} };
                        },
                        else => {
                            return error.InvalidBlock;
                        },
                    }
                },
                .store_block => |*state| {
                    var len: u16 = undefined;
                    if (state.len) |l| {
                        len = l;
                    } else {
                        var saved = self.bs.state;
                        errdefer {
                            self.bs.state = saved;
                        }

                        len = try self.bs.readNumber(u16);
                        if (len != ((try self.bs.readNumber(u16)) ^ 0xFFFF)) {
                            return error.InvalidStoreBlock;
                        }

                        state.len = len;
                    }

                    const tail = @min(buf.len, pos.* + len - state.pos);
                    if (len <= state.pos) {
                        self.state = .{ .end_of_block = .{} };
                        continue;
                    }

                    const value = try self.bs.readBytes(buf[pos.*..tail]);
                    pos.* += value.len;
                    state.pos += value.len;
                },
                .fixed_block => |*state| {
                    fixed: while (pos.* < buf.len) {
                        var length: u9 = undefined;
                        if (state.length) |l| {
                            length = l;
                        } else {
                            length = switch (try state.coding.readLengthLiteral(&self.bs)) {
                                .literal => |literal| {
                                    buf[pos.*] = literal;
                                    pos.* += 1;
                                    continue :fixed;
                                },
                                .end_of_block => {
                                    self.state = .{ .end_of_block =.{} };
                                    continue :outer;
                                },
                                .length => |l| l,
                            };

                            state.length = length;
                        }

                        var dist: u16 = undefined;
                        if (state.distance) |d| {
                            dist = d;
                        } else {
                            dist = try state.coding.readDistance(&self.bs);
                            state.distance = dist;
                        }

                        if (pos.* > 0) {
                            // flush the data so far so it can be read from the window
                            break :outer;
                        }

                        var value = try self.window.read(dist, length, buf[pos.*..], state.pos);
                        pos.* += value.len;
                        state.pos += value.len;

                        if (state.pos >= length) {
                            state.pos = 0;
                            state.length = null;
                            state.distance = null;
                        }
                    }
                },
                .dynamic_block => {
                    self.state = .{
                        .fixed_block = .{
                            .coding = try Coding.readFromBitStream(&self.bs, &self.buf),
                        },
                    };
                },
            }
        }

        return error.UnexpectedEndOfStream;
    }

    pub fn next(self: *Inflator, buf: []u8) !?[]const u8 {
        if (buf.len == 0) {
            return error.BufferTooSmall;
        }

        var pos: usize = 0;
        return nextInternal(self, buf, &pos) catch |err| {
            if (self.closed) {
                return err;
            }

            switch (err) {
                error.UnexpectedEndOfStream => {
                    if (pos > 0) {
                        return self.window.write(buf[0..pos]);
                    }

                    return null;
                },
                else => {
                    return err;
                },
            }
        };
    }

    pub fn write(self: *Inflator, data: []const u8) usize {
        return self.bs.write(data);
    }

    pub fn close(self: *Inflator) void {
        self.closed = true;
    }
};

fn initWasm() *Inflator {
    return std.heap.page_allocator.new(Inflator);
}

fn testInflate(expected: []const u8, compressed: []const u8) !void {
    var inflator = Inflator{};

    var buf: [1<<10]u8 = undefined;
    var len: usize = 0;
    for (compressed) |c| {
        _ = inflator.write(&[_]u8{c});

        while (try inflator.next(buf[len..])) |chunk| {
            len += chunk.len;
        }
    }

    inflator.close();

    while (try inflator.next(buf[len..])) |chunk| {
        len += chunk.len;
    }

    try std.testing.expectEqualStrings(expected, buf[0..len]);
}

test "store only" {
    try testInflate("A SALAD; A SALSA;", &[_]u8{ 0x01, 0x11, 0x00, 0xee, 0xff, 0x41, 0x20, 0x53, 0x41, 0x4c, 0x41, 0x44, 0x3b, 0x20, 0x41, 0x20, 0x53, 0x41, 0x4c, 0x53, 0x41, 0x3b });
    try testInflate("A SALAD; A SALSA;", &[_]u8{ 0x00, 0x11, 0x00, 0xee, 0xff, 0x41, 0x20, 0x53, 0x41, 0x4c, 0x41, 0x44, 0x3b, 0x20, 0x41, 0x20, 0x53, 0x41, 0x4c, 0x53, 0x41, 0x3b, 0x03, 0x00 });
    try testInflate("ABCDEABCD ABCDEABCD", &[_]u8{ 0x01, 0x13, 0x00, 0xec, 0xff, 0x41, 0x42, 0x43, 0x44, 0x45, 0x41, 0x42, 0x43, 0x44, 0x20, 0x41, 0x42, 0x43, 0x44, 0x45, 0x41, 0x42, 0x43, 0x44 });
}

test "fixed only" {
    try testInflate("A SALAD; A SALSA;", &[_]u8{ 0x73, 0x54, 0x08, 0x76, 0xf4, 0x71, 0x74, 0xb1, 0x56, 0x70, 0x04, 0x31, 0x82, 0x1d, 0xad, 0x01 });
    try testInflate("ABCDEABCD ABCDEABCD", &[_]u8{ 0x73, 0x74, 0x72, 0x76, 0x71, 0x75, 0x04, 0x12, 0x0a, 0x20, 0x02, 0xcc, 0x02, 0x00 });
    try testInflate("ABCDEABCD ABCDEABCD", &[_]u8{ 0x73, 0x74, 0x72, 0x76, 0x71, 0x75, 0x04, 0x12, 0x0a, 0x8e, 0x30, 0x16, 0x00 });
}

test "dynamic only" {
    try testInflate("ABCDEABCD ABCDEABCD", &[_]u8{ 0x3d, 0xc6, 0x39, 0x11, 0x00, 0x00, 0x0c, 0x02, 0x30, 0x2b, 0xb5, 0x52, 0x1e, 0xff, 0x96, 0x38, 0x16, 0x96, 0x5c, 0x1e, 0x94, 0xcb, 0x6d, 0x01 });
}
