const Coding = @This();

const std = @import("std");

const BitStream = @import("./bitstream.zig").BitStream;

pub fn Unsigned(comptime bits: comptime_int) type {
    return @Type(.{ .Int = .{ .signedness = .unsigned, .bits = bits } });
}

fn Table(comptime alpha_size: comptime_int, comptime min_code_lengths: comptime_int) type {
    const bits = std.math.log2_int_ceil(usize, alpha_size);

    // const n = 1 << bits;
    const m = 1 << (bits + 1);

    // sum of maximum potential items required for all mapping layers
    const max_items = (1 << (bits + 1)) - 2;

    return struct {
        const alphabet_size = alpha_size;
        const min_read = min_code_lengths;
        const max_bits = bits;

        const min_buf_size = @sizeOf([max_items]Symbol);

        mapping: [bits][]const Symbol = undefined,
        min_bits: u8 = std.math.maxInt(u8),

        const Code = Unsigned(bits);
        const Symbol = ?Code;

        const LengthCounts = struct {
            arr: [m]usize = .{0} ** m,
            len: usize = 0,
        };

        const Self = @This();

        fn init(memory: []align(@alignOf(Symbol)) u8, code_lengths: []const Code) Self {
            if (memory.len < Self.min_buf_size) {
                @panic(std.fmt.comptimePrint("not enough memory for {s}", .{@typeName(Self)}));
            }

            var next_code = prepareNextCodeArray(code_lengths);

            var table = Self{};
            var mapping: [bits][]Symbol = undefined;

            {
                var buf = std.mem.bytesAsSlice(Symbol, memory);
                std.mem.set(Symbol, buf, null);

                mapping[0] = buf[0..0];

                // prepare mapping array
                comptime var i = 1;
                inline while (i < bits) : (i += 1) {
                    const start = (1 << i) - 2;
                    mapping[i] = buf[start..start + (1 << i)];
                }
            }

            var max: usize = 0;
            {
                var i: Code = 0;
                while (i < code_lengths.len) : (i += 1) {
                    const code_length = code_lengths[i];
                    if (code_length == 0) {
                        continue;
                    }

                    if (table.min_bits > code_length) {
                        table.min_bits = @intCast(u8, code_length);
                    }

                    if (max < code_length) {
                        max = code_length;
                    }

                    mapping[code_length][next_code[code_length]] = i;
                    next_code[code_length] += 1;
                }
            }

            {
                var i: u8 = 0;
                while (i < table.min_bits) : (i += 1) {
                    table.mapping[i] = mapping[0];
                }

                while (i <= max) : (i += 1) {
                    table.mapping[i - table.min_bits] = mapping[i];
                }

                while (i < bits) : (i += 1) {
                    table.mapping[i] = mapping[0];
                }
            }

            return table;
        }

        fn countCodeLengths(code_lengths: []const Code) LengthCounts {
            var length_counts = LengthCounts{};
            for (code_lengths) |code_length| {
                if (code_length == 0) {
                    continue;
                }

                if (length_counts.len < code_length) {
                    length_counts.len = code_length;
                }

                length_counts.arr[code_length] += 1;
            }

            length_counts.len += 1;

            return length_counts;
        }

        fn prepareNextCodeArray(code_lengths: []const Code) [m]usize {
            const length_counts = countCodeLengths(code_lengths);

            var next_code = [_]usize{0} ** m;
            var code: usize = 0;

            var i: usize = 1;
            while (i < length_counts.len) : (i += 1) {
                code = (code + length_counts.arr[i - 1]) << 1;
                next_code[i] = code;
            }

            return next_code;
        }

        fn readSymbol(self: Self, bs: *BitStream) !Code {
            var i = self.min_bits - 1;

            var code: Code = try bs.readBits(Code, i, 0);
            var symbol: Symbol = null;

            while (i < max_bits) : (i += 1) {
                code = try bs.readBits(Code, 1, code);
                symbol = self.mapping[i + 1 - self.min_bits][code];
                if (symbol) |s| {
                    return s;
                }
            }

            return error.InvalidCode;
        }
    };
}

pub const LiteralLengthTable = Table(286, 257);
pub const DistanceTable = Table(30, 1);
const CodeLengthTable = Table(18, 4);

pub const min_buf_size = LiteralLengthTable.min_buf_size + CodeLengthTable.min_buf_size;
pub const memory_alignment = @max(@alignOf(LiteralLengthTable.Symbol), @alignOf(CodeLengthTable.Symbol));

literal_length: LiteralLengthTable,
distance: DistanceTable,

pub const fixed = Coding{
    .literal_length = .{
        .mapping = .{
            &[_]LiteralLengthTable.Symbol{
                 256,   257,   258,   259,   260,   261,   262,   263,    264,   265,   266,   267,   268,   269,   270,   271,
                 272,   273,   274,   275,   276,   277,   278,   279,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
            },
            &[_]LiteralLengthTable.Symbol{
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                   0,     1,     2,     3,     4,     5,     6,     7,      8,     9,    10,    11,    12,    13,    14,    15,
                  16,    17,    18,    19,    20,    21,    22,    23,     24,    25,    26,    27,    28,    29,    30,    31,
                  32,    33,    34,    35,    36,    37,    38,    39,     40,    41,    42,    43,    44,    45,    46,    47,
                  48,    49,    50,    51,    52,    53,    54,    55,     56,    57,    58,    59,    60,    61,    62,    63,
                  64,    65,    66,    67,    68,    69,    70,    71,     72,    73,    74,    75,    76,    77,    78,    79,
                  80,    81,    82,    83,    84,    85,    86,    87,     88,    89,    90,    91,    92,    93,    94,    95,
                  96,    97,    98,    99,   100,   101,   102,   103,    104,   105,   106,   107,   108,   109,   110,   111,
                 112,   113,   114,   115,   116,   117,   118,   119,    120,   121,   122,   123,   124,   125,   126,   127,
                 128,   129,   130,   131,   132,   133,   134,   135,    136,   137,   138,   139,   140,   141,   142,   143,
                 280,   281,   282,   283,   284,   285,   286,   287,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
            },
            &[_]LiteralLengthTable.Symbol{
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                null,  null,  null,  null,  null,  null,  null,  null,   null,  null,  null,  null,  null,  null,  null,  null,
                 144,   145,   146,   147,   148,   149,   150,   151,    152,   153,   154,   155,   156,   157,   158,   159,
                 160,   161,   162,   163,   164,   165,   166,   167,    168,   169,   170,   171,   172,   173,   174,   175,
                 176,   177,   178,   179,   180,   181,   182,   183,    184,   185,   186,   187,   188,   189,   190,   191,
                 192,   193,   194,   195,   196,   197,   198,   199,    200,   201,   202,   203,   204,   205,   206,   207,
                 208,   209,   210,   211,   212,   213,   214,   215,    216,   217,   218,   219,   220,   221,   222,   223,
                 224,   225,   226,   227,   228,   229,   230,   231,    232,   233,   234,   235,   236,   237,   238,   239,
                 240,   241,   242,   243,   244,   245,   246,   247,    248,   249,   250,   251,   252,   253,   254,   255,
            },
            &[_]LiteralLengthTable.Symbol{},
            &[_]LiteralLengthTable.Symbol{},
            &[_]LiteralLengthTable.Symbol{},
            &[_]LiteralLengthTable.Symbol{},
            &[_]LiteralLengthTable.Symbol{},
            &[_]LiteralLengthTable.Symbol{},
        },
        .min_bits = 7,
    },
    .distance = .{
        .mapping = .{
            &[_]CodeLengthTable.Symbol{
                 0,  1,  2,  3,  4,  5,  6,  7,
                 8,  9, 10, 11, 12, 13, 14, 15,
                16, 17, 18, 19, 20, 21, 22, 23,
                24, 25, 26, 27, 28, 29, 30, 31,
            },
            &[_]CodeLengthTable.Symbol{},
            &[_]CodeLengthTable.Symbol{},
            &[_]CodeLengthTable.Symbol{},
            &[_]CodeLengthTable.Symbol{},
        },
        .min_bits = 5,
    },
};

const code_length_order = [19]u5{16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15};

fn readCodeLengthTable(bs: *BitStream, hclen: u4, buf: []align(2) u8) !CodeLengthTable {
    var code_lengths = [_]CodeLengthTable.Code{0} ** 19;

    var i: usize = 0;
    while (i < @intCast(usize, hclen) + CodeLengthTable.min_read) : (i += 1) {
        code_lengths[code_length_order[i]] = try bs.readNumber(u3);
    }

    return CodeLengthTable.init(buf, &code_lengths);
}

fn readTable(comptime TTable: type, bs: *BitStream, code_length_table: CodeLengthTable, read_count: TTable.Code, memory: []align(@alignOf(TTable.Symbol)) u8) !TTable {
    var codes = [_]TTable.Code{0} ** 286;

    var staged_code: TTable.Code = 0;
    var code_count: TTable.Code = 0;

    var i: usize = 0;
    while (i < read_count + TTable.min_read) : (i += 1) {
        if (code_count == 0) {
            const code = try code_length_table.readSymbol(bs);
            if (code <= 15) {
                staged_code = @intCast(TTable.Code, code);
                code_count = 1;
            } else if (code == 16) {
                code_count = @intCast(TTable.Code, try bs.readNumber(u2)) + 3;
            } else if (code == 17) {
                staged_code = 0;
                code_count = @intCast(TTable.Code, try bs.readNumber(u3)) + 3;
            } else if (code == 18) {
                staged_code = 0;
                code_count = @intCast(TTable.Code, try bs.readNumber(u7)) + 11;
            } else {
                return error.InvalidSymbol;
            }
        }

        code_count -= 1;
        codes[i] = staged_code;
    }

    return TTable.init(memory, codes[0..TTable.alphabet_size]);
}

pub fn readFromBitStream(bs: *BitStream, memory: []align(memory_alignment) u8) !Coding {
    if (memory.len < Coding.min_buf_size) {
        @panic(std.fmt.comptimePrint("not enough memory for {s}", .{@typeName(Coding)}));
    }

    const state = bs.state;
    errdefer {
        bs.state = state;
    }

    const hlit = try bs.readNumber(u5);
    const hdist = try bs.readNumber(u5);
    const hclen = try bs.readNumber(u4);

    var clbuf: [CodeLengthTable.min_buf_size]u8 align(2) = undefined;
    const table = try readCodeLengthTable(bs, hclen, &clbuf);
    return Coding{
        .literal_length = try readTable(LiteralLengthTable, bs, table, hlit, memory),
        .distance = try readTable(DistanceTable, bs, table, hdist, memory[LiteralLengthTable.min_buf_size..]),
    };
}

const LengthLiteral = union(enum) {
    length: u9,
    literal: u8,
    end_of_block: void,
};

fn readExtraBits(bs: *BitStream, code: u5, group_size: u3, base: u2) !u16 {
    const extra_bits = @intCast(u4, @max(0, @divTrunc((@intCast(i6, code) - group_size), group_size)));
    if (extra_bits == 0) {
        return base + code;
    }

    const group_index = code % group_size;
    return base + code - ((@intCast(u16, extra_bits) + 1) * group_size) - group_index + ((@intCast(u16, 1) << extra_bits) * (group_size + group_index)) + try bs.readNumberWidth(u16, extra_bits);
}

pub fn readLengthLiteral(self: *Coding, bs: *BitStream) !LengthLiteral {
    var state = bs.state;
    errdefer {
        bs.state = state;
    }

    var ll = try self.literal_length.readSymbol(bs);
    if (ll < 256) {
        return LengthLiteral{ .literal = @intCast(u8, ll) };
    }

    if (ll == 256) {
        return LengthLiteral{ .end_of_block = {} };
    }

    if (ll > 285) {
        return error.InvalidSymbol;
    }

    if (ll > 284) {
        return LengthLiteral{ .length = 258 };
    }

    return LengthLiteral{ .length = @intCast(u9, try readExtraBits(bs, @intCast(u5, ll - 257), 4, 3)) };
}

pub fn readDistance(self: *Coding, bs: *BitStream) !u16 {
    var state = bs.state;
    errdefer {
        bs.state = state;
    }

    return try readExtraBits(bs, @intCast(u5, try self.distance.readSymbol(bs)), 2, 1);
}

// const test_code_length_table = CodeLengthTable.init(&[_]CodeLengthTable.Code{4,3,0,2,3,0,0,0,0,0,0,0,0,0,0,0,4,3,2});
const test_code_length_table = CodeLengthTable{
    .mapping = .{
        &[_]CodeLengthTable.Symbol{3, 18, null, null},
        &[_]CodeLengthTable.Symbol{null, null, null, null, 1, 4, 17, null},
        &[_]CodeLengthTable.Symbol{null, null, null, null, null, null, null, null, null, null, null, null, null, null, 0, 16},
        &[_]CodeLengthTable.Symbol{},
        &[_]CodeLengthTable.Symbol{},
    },
    .min_bits = 2,
};

fn expectEqualTables(comptime TTable: type, expected: TTable, actual: TTable) !void {
    comptime var i = 0;
    inline while (i < @typeInfo(@typeInfo(TTable).Struct.fields[0].field_type).Array.len) : (i += 1) {
        try std.testing.expectEqualSlices(TTable.Symbol, expected.mapping[i], actual.mapping[i]);
    }

    try std.testing.expectEqual(expected.min_bits, actual.min_bits);
}

test "read code length table" {
    var bs = BitStream.initWithData(&[_]u8{0x9C, 0x08, 0x00, 0x00, 0x06, 0x01, 0x18}) catch unreachable;

    var buf: [CodeLengthTable.min_buf_size]u8 align(2) = undefined;
    try expectEqualTables(CodeLengthTable, test_code_length_table, try Coding.readCodeLengthTable(&bs, 14, &buf));
}

const test_coding = Coding{
    .literal_length = LiteralLengthTable{
        .mapping = .{
            &[_]LiteralLengthTable.Symbol{66, 67, 68, 69, 256, 258, 263, null},
            &[_]LiteralLengthTable.Symbol{null, null, null, null, null, null, null, null, null, null, null, null, null, null, 32, 65},
            &[_]LiteralLengthTable.Symbol{},
            &[_]LiteralLengthTable.Symbol{},
            &[_]LiteralLengthTable.Symbol{},
            &[_]LiteralLengthTable.Symbol{},
            &[_]LiteralLengthTable.Symbol{},
            &[_]LiteralLengthTable.Symbol{},
            &[_]LiteralLengthTable.Symbol{},
        },
        .min_bits = 3,
    },
    .distance = DistanceTable{
        .mapping = .{
            &[_]DistanceTable.Symbol{4, 6},
            &[_]DistanceTable.Symbol{},
            &[_]DistanceTable.Symbol{},
            &[_]DistanceTable.Symbol{},
            &[_]DistanceTable.Symbol{},
        },
        .min_bits = 1,
    },
};

test "read literal length table" {
    var bs = BitStream.initWithData(&[_]u8{0x56, 0x6A, 0xA5, 0x3C, 0xFE, 0x2D, 0x71, 0x2C, 0x00}) catch unreachable;

    var buf: [LiteralLengthTable.min_buf_size]u8 align(@alignOf(LiteralLengthTable.Symbol)) = undefined;
    try expectEqualTables(LiteralLengthTable, test_coding.literal_length, try Coding.readTable(LiteralLengthTable, &bs, test_code_length_table, 7, &buf));
}

test "read distance table" {
    var bs = BitStream.initWithData(&[_]u8{0x4B, 0x2E}) catch unreachable;

    var buf: [DistanceTable.min_buf_size]u8 align(@alignOf(DistanceTable.Symbol)) = undefined;
    try expectEqualTables(DistanceTable, test_coding.distance, try Coding.readTable(DistanceTable, &bs, test_code_length_table, 6, &buf));
}

test "read dynamic coding" {
    var bs = BitStream.initWithData(&[_]u8{0xC7, 0x38, 0x27, 0x02, 0x00, 0x80, 0x41, 0x00, 0x66, 0xA5, 0x56, 0xCA, 0xE3, 0xDF, 0x12, 0xC7, 0xC2, 0x92, 0xCB}) catch unreachable;

    var buf: [Coding.min_buf_size]u8 align(memory_alignment) = undefined;
    const coding = try Coding.readFromBitStream(&bs, &buf);
    try expectEqualTables(LiteralLengthTable, test_coding.literal_length, coding.literal_length);
    try expectEqualTables(DistanceTable, test_coding.distance, coding.distance);
}
