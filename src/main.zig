const std = @import("std");

const Token = union(enum) {
    lparen,
    rparen,
    ident: []u8,
    string: []u8,
    char: u8,
    int: i64,
    float: f64,

    fn deinit(self: Token, allocator: std.mem.Allocator) void {
        switch (self) {
            .ident => |ident| allocator.free(ident),
            .string => |string| allocator.free(string),
            else => {},
        }
    }
};

const TokenizerError = error{
    UnmatchedDoubleQuote,
    UnmatchedSingleQuote,
    InvalidToken,
};

fn todo(message: []const u8) noreturn {
    @panic(message);
}

const Tokenizer = struct {
    source: []const u8,
    idx: usize,

    fn init(source: []const u8) Tokenizer {
        return .{
            .source = source,
            .idx = 0,
        };
    }

    fn peekChar(self: *Tokenizer) ?u8 {
        return if (self.bytesLeft() == 0) null else self.source[self.idx];
    }

    fn skipWhitespace(self: *Tokenizer) void {
        for (self.source[self.idx..]) |c| {
            if (!std.ascii.isWhitespace(c)) {
                break;
            }
            self.idx += 1;
        }
    }

    fn bytesLeft(self: *const Tokenizer) usize {
        return self.source.len - self.idx;
    }

    fn isIdentChar(c: u8) bool {
        return switch (c) {
            '!', '#'...'&', '*'...'~' => true,
            else => false,
        };
    }

    fn next(self: *Tokenizer, allocator: std.mem.Allocator) (std.mem.Allocator.Error || TokenizerError)!?Token {
        self.skipWhitespace();

        var advance_by: usize = 1;
        const token: Token = switch (self.peekChar() orelse return null) {
            '(' => .{ .lparen = {} },
            ')' => .{ .rparen = {} },
            '\'' => {
                todo("Char");
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
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const status = gpa.deinit();
        _ = status;
    }

    const source =
        \\(function factorial
        \\    ((int n))
        \\    int
        \\    (
        \\        (return (if (== n 0) 1 (* n (factorial (- n 1)))))
        \\    )
        \\)
        \\
        \\(function main
        \\    ((int argc) ((* (* char)) argv))
        \\    int
        \\    (
        \\        (print "Hello, world!")
        \\        (return (factorial 10))
        \\    )
        \\)
        \\
    ;

    var tokenizer = Tokenizer.init(source);

    while (try tokenizer.next(allocator)) |*token| {
        std.debug.print("Token: {any}\n", .{token});
        token.deinit(allocator);
    }
}
