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

fn tokenizeChar(self: *Self) Error!u8 {
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
    return char;
}

fn tokenizeString(self: *Self, allocator: std.mem.Allocator, advance_by: *usize) (std.mem.Allocator.Error || Error)![]u8 {
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
    advance_by.* = size + 1;
    return allocator.dupe(u8, self.source[self.idx .. self.idx + size]);
}

fn tokenizeIdent(self: *Self, allocator: std.mem.Allocator, advance_by: *usize) (std.mem.Allocator.Error || Error)![]u8 {
    var size: usize = 1;
    for (self.source[self.idx + 1 ..]) |c| {
        if (!isIdentChar(c)) {
            break;
        }
        size += 1;
    }
    advance_by.* = size;
    return allocator.dupe(u8, self.source[self.idx .. self.idx + size]);
}

pub fn next(self: *Self, allocator: std.mem.Allocator) (std.mem.Allocator.Error || Error)!?Token {
    self.skipWhitespace();

    var advance_by: usize = 1;
    const token: Token = switch (self.peekChar() orelse return null) {
        '(' => .{ .lparen = {} },
        ')' => .{ .rparen = {} },
        '\'' => .{ .char = try self.tokenizeChar() },
        '"' => .{ .string = try self.tokenizeString(allocator, &advance_by) },
        '!', '#'...'&', '*', '+', ',', '-', '.', '/', ':'...'~' => .{ .ident = try self.tokenizeIdent(allocator, &advance_by) },
        '0'...'9' => blk: {
            // TODO: Handle other bases and decimal numbers
            var number: i64 = 0;
            const start_idx = self.idx;
            advance_by = 0;
            while (self.peekChar()) |c| : (self.idx += 1) {
                if (!std.ascii.isDigit(c)) {
                    break;
                }
                number = 10 * number + c - '0';
            }

            // TODO: Scientific notation?
            if (self.peekChar() != '.') {
                break :blk .{ .int = number };
            }
            self.idx += 1;
            while (self.peekChar()) |c| : (self.idx += 1) {
                if (!std.ascii.isDigit(c)) {
                    break;
                }
            }
            const float = std.fmt.parseFloat(f64, self.source[start_idx..self.idx]) catch unreachable;
            break :blk .{ .float = float };
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

fn expectFloat(allocator: std.mem.Allocator, token: ?Token, expected_float: f64) !void {
    defer {
        if (token) |tok| {
            tok.deinit(allocator);
        }
    }
    if (token) |tok| {
        return switch (tok) {
            .float => |float| {
                if (float != expected_float) {
                    return error.WrongFloat;
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

test "ints" {
    const allocator = std.testing.allocator;
    const source =
        \\ 0 10 0.0 4.2 20.0 0.001 'c' 
    ;
    var tokenizer = Self.init(source);

    try expectInt(allocator, try tokenizer.next(allocator), 0);
    try expectInt(allocator, try tokenizer.next(allocator), 10);
    try expectFloat(allocator, try tokenizer.next(allocator), 0.0);
    try expectFloat(allocator, try tokenizer.next(allocator), 4.2);
    try expectFloat(allocator, try tokenizer.next(allocator), 20.0);
    try expectFloat(allocator, try tokenizer.next(allocator), 0.001);
    try expectChar(allocator, try tokenizer.next(allocator), 'c');
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
