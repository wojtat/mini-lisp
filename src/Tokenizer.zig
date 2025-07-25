const std = @import("std");

const Self = @This();

const Token = union(enum) {
    lparen,
    rparen,
    ident: []u8,
    string: []u8,
    char: u8,
    int: i64,
    float: f64,

    pub fn deinit(self: Token, allocator: std.mem.Allocator) void {
        switch (self) {
            .ident => |ident| allocator.free(ident),
            .string => |string| allocator.free(string),
            else => {},
        }
    }
};

const Error = error{
    UnmatchedDoubleQuote,
    UnmatchedSingleQuote,
    InvalidToken,
};

source: []const u8,
idx: usize,

pub fn init(source: []const u8) Self {
    return .{
        .source = source,
        .idx = 0,
    };
}

fn peekChar(self: *Self) ?u8 {
    return if (self.bytesLeft() == 0) null else self.source[self.idx];
}

fn skipWhitespace(self: *Self) void {
    for (self.source[self.idx..]) |c| {
        if (!std.ascii.isWhitespace(c)) {
            break;
        }
        self.idx += 1;
    }
}

fn bytesLeft(self: *const Self) usize {
    return self.source.len - self.idx;
}

fn isIdentChar(c: u8) bool {
    return switch (c) {
        '!', '#'...'&', '*'...'~' => true,
        else => false,
    };
}

pub fn next(self: *Self, allocator: std.mem.Allocator) (std.mem.Allocator.Error || Error)!?Token {
    self.skipWhitespace();

    var advance_by: usize = 1;
    const token: Token = switch (self.peekChar() orelse return null) {
        '(' => .{ .lparen = {} },
        ')' => .{ .rparen = {} },
        '\'' => blk: {
            // TODO: Escaping
            self.idx += 1;
            const char = self.source[self.idx];
            self.idx += 1;
            if (self.peekChar()) |c| {
                if (c != '\'') {
                    return error.UnmatchedSingleQuote;
                }
            } else {
                return error.UnmatchedSingleQuote;
            }
            break :blk .{ .char = char };
        },
        '"' => blk: {
            // TODO: Escaping
            self.idx += 1;
            var size: usize = 0;
            for (self.source[self.idx..]) |c| {
                if (c == '"') {
                    break;
                }
                size += 1;
            } else {
                // We reached the end of the source, but no end of string was found
                return error.UnmatchedDoubleQuote;
            }
            const string = try allocator.alloc(u8, size);
            @memcpy(string, self.source[self.idx .. self.idx + size]);
            advance_by = size + 1;
            break :blk .{ .string = string };
        },
        '!', '#'...'&', '*', '+', ',', '-', '.', '/', ':'...'~' => blk: {
            var size: usize = 1;
            for (self.source[self.idx + 1 ..]) |c| {
                if (!isIdentChar(c)) {
                    break;
                }
                size += 1;
            }
            const ident = try allocator.alloc(u8, size);
            @memcpy(ident, self.source[self.idx .. self.idx + size]);
            advance_by = size;
            break :blk .{ .ident = ident };
        },
        '0'...'9' => blk: {
            // TODO: Handle other bases and decimal numbers
            var number: i64 = 0;
            while (self.peekChar()) |c| : (self.idx += 1) {
                if (!std.ascii.isDigit(c)) {
                    break;
                }
                number = 10 * number + c - '0';
            }

            advance_by = 0;
            break :blk .{ .int = number };
        },
        else => return error.InvalidToken,
    };

    self.idx += advance_by;

    return token;
}

fn expectLparen(allocator: std.mem.Allocator, token: ?Token) !void {
    defer {
        if (token) |tok| {
            tok.deinit(allocator);
        }
    }
    if (token) |tok| {
        return switch (tok) {
            .lparen => {},
            else => error.WrongToken,
        };
    } else {
        return error.NoToken;
    }
}

fn expectRparen(allocator: std.mem.Allocator, token: ?Token) !void {
    defer {
        if (token) |tok| {
            tok.deinit(allocator);
        }
    }
    if (token) |tok| {
        return switch (tok) {
            .rparen => {},
            else => error.WrongToken,
        };
    } else {
        return error.NoToken;
    }
}

fn expectChar(allocator: std.mem.Allocator, token: ?Token, expected_char: u8) !void {
    defer {
        if (token) |tok| {
            tok.deinit(allocator);
        }
    }
    if (token) |tok| {
        return switch (tok) {
            .char => |char| {
                if (char != expected_char) {
                    return error.WrongChar;
                }
            },
            else => error.WrongToken,
        };
    } else {
        return error.NoToken;
    }
}

fn expectInt(allocator: std.mem.Allocator, token: ?Token, expected_int: i64) !void {
    defer {
        if (token) |tok| {
            tok.deinit(allocator);
        }
    }
    if (token) |tok| {
        return switch (tok) {
            .int => |int| {
                if (int != expected_int) {
                    return error.WrongInt;
                }
            },
            else => error.WrongToken,
        };
    } else {
        return error.NoToken;
    }
}

fn expectFloat(allocator: std.mem.Allocator, token: ?Token, expected_int: i64) !void {
    defer {
        if (token) |tok| {
            tok.deinit(allocator);
        }
    }
    if (token) |tok| {
        return switch (tok) {
            .int => |int| {
                if (int != expected_int) {
                    return error.WrongInt;
                }
            },
            else => error.WrongToken,
        };
    } else {
        return error.NoToken;
    }
}

fn expectIdent(allocator: std.mem.Allocator, token: ?Token, expected_ident: []const u8) !void {
    defer {
        if (token) |tok| {
            tok.deinit(allocator);
        }
    }
    if (token) |tok| {
        return switch (tok) {
            .ident => |ident| {
                if (!std.mem.eql(u8, ident, expected_ident)) {
                    return error.WrongIdent;
                }
            },
            else => error.WrongToken,
        };
    } else {
        return error.NoToken;
    }
}

fn expectString(allocator: std.mem.Allocator, token: ?Token, expected_string: []const u8) !void {
    defer {
        if (token) |tok| {
            tok.deinit(allocator);
        }
    }
    if (token) |tok| {
        return switch (tok) {
            .string => |string| {
                if (!std.mem.eql(u8, string, expected_string)) {
                    return error.WrongString;
                }
            },
            else => error.WrongToken,
        };
    } else {
        return error.NoToken;
    }
}

fn expectNoToken(allocator: std.mem.Allocator, token: ?Token) !void {
    defer {
        if (token) |tok| {
            tok.deinit(allocator);
        }
    }
    if (token) |_| {
        return error.HasToken;
    }
}

test "empty source" {
    const allocator = std.testing.allocator;
    const source =
        \\
    ;
    var tokenizer = Self.init(source);

    try expectNoToken(allocator, try tokenizer.next(allocator));
    // Still empty
    try expectNoToken(allocator, try tokenizer.next(allocator));
}

test "parentheses" {
    const allocator = std.testing.allocator;
    const source =
        \\(())()()
    ;
    var tokenizer = Self.init(source);

    try expectLparen(allocator, try tokenizer.next(allocator));
    try expectLparen(allocator, try tokenizer.next(allocator));
    try expectRparen(allocator, try tokenizer.next(allocator));
    try expectRparen(allocator, try tokenizer.next(allocator));
    try expectLparen(allocator, try tokenizer.next(allocator));
    try expectRparen(allocator, try tokenizer.next(allocator));
    try expectLparen(allocator, try tokenizer.next(allocator));
    try expectRparen(allocator, try tokenizer.next(allocator));
    try expectNoToken(allocator, try tokenizer.next(allocator));
}

test "idents" {
    const allocator = std.testing.allocator;
    const source =
        \\(A (hello world) )
    ;
    var tokenizer = Self.init(source);

    try expectLparen(allocator, try tokenizer.next(allocator));
    try expectIdent(allocator, try tokenizer.next(allocator), "A");
    try expectLparen(allocator, try tokenizer.next(allocator));
    try expectIdent(allocator, try tokenizer.next(allocator), "hello");
    try expectIdent(allocator, try tokenizer.next(allocator), "world");
    try expectRparen(allocator, try tokenizer.next(allocator));
    try expectRparen(allocator, try tokenizer.next(allocator));
    try expectNoToken(allocator, try tokenizer.next(allocator));
}

test "strings" {
    const allocator = std.testing.allocator;
    const source =
        \\("A" ("hello" "world") )
    ;
    var tokenizer = Self.init(source);

    try expectLparen(allocator, try tokenizer.next(allocator));
    try expectString(allocator, try tokenizer.next(allocator), "A");
    try expectLparen(allocator, try tokenizer.next(allocator));
    try expectString(allocator, try tokenizer.next(allocator), "hello");
    try expectString(allocator, try tokenizer.next(allocator), "world");
    try expectRparen(allocator, try tokenizer.next(allocator));
    try expectRparen(allocator, try tokenizer.next(allocator));
    try expectNoToken(allocator, try tokenizer.next(allocator));
}
