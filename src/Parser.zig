const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");

pub const List = struct {
    elements: std.ArrayList(Expression),

    pub fn deinit(self: List, allocator: std.mem.Allocator) void {
        for (self.elements.items) |element| {
            element.deinit(allocator);
        }
        self.elements.deinit();
    }

    pub fn dupe(self: List, allocator: std.mem.Allocator) std.mem.Allocator.Error!List {
        var elements = std.ArrayList(Expression).init(allocator);
        errdefer {
            for (elements.items) |element| {
                element.deinit(allocator);
            }
            elements.deinit();
        }
        for (self.elements.items) |element| {
            const duped_element = try element.dupe(allocator);
            errdefer duped_element.deinit(allocator);
            try elements.append(duped_element);
        }
        return .{ .elements = elements };
    }

    pub fn debugPrint(self: List) void {
        std.debug.print("(", .{});
        for (self.elements.items) |element| {
            element.debugPrint();
        }
        std.debug.print(") ", .{});
    }
};

pub const Expression = union(enum) {
    int: i64,
    float: f64,
    char: u8,
    string: []const u8,
    ident: []const u8,
    list: List,

    pub fn deinit(self: Expression, allocator: std.mem.Allocator) void {
        switch (self) {
            .string => |string| allocator.free(string),
            .ident => |ident| allocator.free(ident),
            .list => |list| list.deinit(allocator),
            else => {},
        }
    }

    pub fn dupe(self: Expression, allocator: std.mem.Allocator) std.mem.Allocator.Error!Expression {
        return switch (self) {
            .int, .float, .char => self,
            .string => |string| .{ .string = try allocator.dupe(u8, string) },
            .ident => |ident| .{ .ident = try allocator.dupe(u8, ident) },
            .list => |list| .{ .list = try list.dupe(allocator) },
        };
    }

    pub fn debugPrint(self: Expression) void {
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

const Error = error{ UnexpectedEndOfInput, UnexpectedRparen };

const Self = @This();

allocator: std.mem.Allocator,
tokenizer: Tokenizer,

pub fn init(allocator: std.mem.Allocator, source: []const u8) Self {
    return .{
        .allocator = allocator,
        .tokenizer = Tokenizer.init(source),
    };
}

fn parseList(self: *Self) (std.mem.Allocator.Error || Tokenizer.Error || Error)!List {
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

pub fn parseExpression(self: *Self) (std.mem.Allocator.Error || Tokenizer.Error || Error)!?Expression {
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

test "parse empty" {
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;
    const source =
        \\
    ;
    var parser = Self.init(allocator, source);

    try expect(try parser.parseExpression() == null);
    // Still empty
    try expect(try parser.parseExpression() == null);
}

test "parse complicated" {
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;
    const source =
        \\("hello" 42 () 'c' 3.14)
    ;
    var parser = Self.init(allocator, source);

    const maybe_expression = try parser.parseExpression();
    try expect(maybe_expression != null);
    const expression = maybe_expression.?;
    defer expression.deinit(allocator);
    switch (expression) {
        .list => |list| {
            try expect(list.elements.items.len == 5);
            try expect(switch (list.elements.items[0]) {
                .string => |string| std.mem.eql(u8, string, "hello"),
                else => false,
            });
            try expect(switch (list.elements.items[1]) {
                .int => |int| int == 42,
                else => false,
            });
            try expect(switch (list.elements.items[2]) {
                .list => |inner| inner.elements.items.len == 0,
                else => false,
            });
            try expect(switch (list.elements.items[3]) {
                .char => |char| char == 'c',
                else => false,
            });
            try expect(switch (list.elements.items[4]) {
                .float => |float| float == 3.14,
                else => false,
            });
        },
        else => return error.ExpectedList,
    }
    try expect(try parser.parseExpression() == null);
}
