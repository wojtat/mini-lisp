const std = @import("std");
const Allocator = std.mem.Allocator;

const Self = @This();

const Token = union(enum) {
    lparen,
    rparen,
    ident: []u8,
    string: []u8,
    bool: bool,
    char: u8,
    int: i64,
    float: f64,

    pub fn deinit(self: Token, allocator: Allocator) void {
        switch (self) {
            .ident => |ident| allocator.free(ident),
            .string => |string| allocator.free(string),
            else => {},
        }
    }
};

pub const Error = error{
    UnmatchedDoubleQuote,
    UnmatchedSingleQuote,
    InvalidEscape,
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

fn charToEscape(char: u8) ?u8 {
    return switch (char) {
        '0' => 0,
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        '\'' => '\'',
        '\"' => '\"',
        else => null,
    };
}

fn handleEscape(self: *Self) Error!u8 {
    self.idx += 1;
    const next_char = self.peekChar() orelse return error.InvalidEscape;
    if (charToEscape(next_char)) |escaped| {
        // It's a simple character escape
        return escaped;
    } else {
        // It must be a byte escape
        if (next_char != 'x') {
            return error.InvalidEscape;
        }
        self.idx += 1;
        const escaped_or_err = std.fmt.parseInt(u8, self.source[self.idx .. self.idx + 2], 16);
        self.idx += 1;
        return escaped_or_err catch return error.InvalidEscape;
    }
}

fn tokenizeChar(self: *Self) Error!u8 {
    self.idx += 1;
    var char = self.source[self.idx];
    if (char == '\\') {
        char = try self.handleEscape();
    }
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

fn tokenizeString(self: *Self, allocator: Allocator, advance_by: *usize) (Allocator.Error || Error)![]u8 {
    self.idx += 1;
    var string = std.ArrayList(u8).init(allocator);
    errdefer string.deinit();
    while (self.peekChar()) |c| : (self.idx += 1) {
        if (c == '"') {
            break;
        }
        var char = c;
        if (c == '\\') {
            char = try self.handleEscape();
        }
        try string.append(char);
    } else {
        // We reached the end of the source, but no end of string was found
        return error.UnmatchedDoubleQuote;
    }
    advance_by.* = 1;
    return string.toOwnedSlice();
}

fn tokenizeIdent(self: *Self, allocator: Allocator, advance_by: *usize) (Allocator.Error || Error)!Token {
    var size: usize = 1;
    for (self.source[self.idx + 1 ..]) |c| {
        if (!isIdentChar(c)) {
            break;
        }
        size += 1;
    }
    advance_by.* = size;
    const slice = self.source[self.idx .. self.idx + size];
    if (std.mem.eql(u8, slice, "true")) {
        return .{ .bool = true };
    } else if (std.mem.eql(u8, slice, "false")) {
        return .{ .bool = false };
    }
    return .{ .ident = try allocator.dupe(u8, slice) };
}

pub fn next(self: *Self, allocator: Allocator) (Allocator.Error || Error)!?Token {
    self.skipWhitespace();

    var advance_by: usize = 1;
    const token: Token = switch (self.peekChar() orelse return null) {
        '(' => .{ .lparen = {} },
        ')' => .{ .rparen = {} },
        '\'' => .{ .char = try self.tokenizeChar() },
        '"' => .{ .string = try self.tokenizeString(allocator, &advance_by) },
        '!', '#'...'&', '*', '+', ',', '-', '.', '/', ':'...'~' => try self.tokenizeIdent(allocator, &advance_by),
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

fn expectLparen(allocator: Allocator, token: ?Token) !void {
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

fn expectRparen(allocator: Allocator, token: ?Token) !void {
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

fn expectChar(allocator: Allocator, token: ?Token, expected_char: u8) !void {
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

fn expectInt(allocator: Allocator, token: ?Token, expected_int: i64) !void {
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

fn expectFloat(allocator: Allocator, token: ?Token, expected_float: f64) !void {
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

fn expectIdent(allocator: Allocator, token: ?Token, expected_ident: []const u8) !void {
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

fn expectString(allocator: Allocator, token: ?Token, expected_string: []const u8) !void {
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

fn expectNoToken(allocator: Allocator, token: ?Token) !void {
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

test "numbers" {
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

test "escaping" {
    const allocator = std.testing.allocator;
    const source =
        \\("A\n" ("\rhello" "wor\x20ld" '\"') )
    ;
    var tokenizer = Self.init(source);

    try expectLparen(allocator, try tokenizer.next(allocator));
    try expectString(allocator, try tokenizer.next(allocator), "A\n");
    try expectLparen(allocator, try tokenizer.next(allocator));
    try expectString(allocator, try tokenizer.next(allocator), "\rhello");
    try expectString(allocator, try tokenizer.next(allocator), "wor ld");
    try expectChar(allocator, try tokenizer.next(allocator), '"');
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
