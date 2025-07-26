const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");

const List = struct {
    elements: std.ArrayList(Expression),

    fn deinit(self: List, allocator: std.mem.Allocator) void {
        for (self.elements.items) |element| {
            element.deinit(allocator);
        }
        self.elements.deinit();
    }

    fn debugPrint(self: List) void {
        std.debug.print("(", .{});
        for (self.elements.items) |element| {
            element.debugPrint();
        }
        std.debug.print(") ", .{});
    }
};

const Expression = union(enum) {
    int: i64,
    float: f64,
    char: u8,
    string: []u8,
    ident: []u8,
    list: List,

    fn deinit(self: Expression, allocator: std.mem.Allocator) void {
        switch (self) {
            .string => |string| allocator.free(string),
            .ident => |ident| allocator.free(ident),
            .list => |list| list.deinit(allocator),
            else => {},
        }
    }

    fn debugPrint(self: Expression) void {
        switch (self) {
            .int => |int| std.debug.print("{d} ", .{int}),
            .float => |float| std.debug.print("{d} ", .{float}),
            .char => |char| std.debug.print("{c} ", .{char}),
            .string => |string| std.debug.print("\"{s}\" ", .{string}),
            .ident => |ident| std.debug.print("{s} ", .{ident}),
            .list => |list| {
                list.debugPrint();
            },
        }
    }
};

const Parser = struct {
    allocator: std.mem.Allocator,
    tokenizer: Tokenizer,

    fn init(allocator: std.mem.Allocator, source: []const u8) Parser {
        return .{
            .allocator = allocator,
            .tokenizer = Tokenizer.init(source),
        };
    }

    fn parseList(self: *Parser) !List {
        var elements = std.ArrayList(Expression).init(self.allocator);
        errdefer {
            for (elements.items) |element| {
                element.deinit(self.allocator);
            }
            elements.deinit();
        }
        while (true) {
            const element = if (self.parseExpression()) |expression|
                expression orelse return error.UnexpectedEndOfInput
            else |err| switch (err) {
                error.UnexpectedRparen => break,
                else => |leftover_err| return leftover_err,
            };
            try elements.append(element);
        }
        return .{ .elements = elements };
    }

    fn parseExpression(self: *Parser) anyerror!?Expression {
        const maybe_token = try self.tokenizer.next(self.allocator);
        const token = maybe_token orelse return null;

        // Technically shouldn't be needed, but this is more future-proof
        errdefer token.deinit(self.allocator);

        const expression: Expression = switch (token) {
            .lparen => .{ .list = try self.parseList() },
            .rparen => return error.UnexpectedRparen,
            .char => |char| .{ .char = char },
            .int => |int| .{ .int = int },
            .float => |float| .{ .float = float },
            .string => |string| .{ .string = string },
            .ident => |ident| .{ .ident = ident },
        };
        return expression;
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
        \\(define factorial (lambda (n)
        \\    (if (== n 0)
        \\        1
        \\        (* n (factorial (- n 1))))))
    ;

    var parser = Parser.init(allocator, source);
    const expression = (try parser.parseExpression()) orelse return error.UnexpectedEndOfInput;
    defer expression.deinit(allocator);
    expression.debugPrint();
    std.debug.print("\n", .{});
}
