const std = @import("std");
const Allocator = std.mem.Allocator;
const Tokenizer = @import("Tokenizer.zig");
const LinkedList = @import("LinkedList.zig");
const LinkedListWrapper = LinkedList.Wrapper;

pub const Pair = struct {
    left: *Expression,
    right: *Expression,

    pub fn init(allocator: Allocator, left: Expression, right: Expression) Allocator.Error!Pair {
        // If the right is a list, construct a list instead
        std.debug.assert(switch (right) {
            .list => false,
            else => true,
        });
        const leftp = try allocator.create(Expression);
        errdefer allocator.destroy(leftp);
        const rightp = try allocator.create(Expression);
        leftp.* = left;
        rightp.* = right;
        return .{ .left = leftp, .right = rightp };
    }

    fn deinit(self: Pair, allocator: Allocator) void {
        self.left.deinit(allocator);
        allocator.destroy(self.left);
        self.right.deinit(allocator);
        allocator.destroy(self.right);
    }

    fn dupe(self: Pair, allocator: Allocator) Allocator.Error!Pair {
        const left = try self.left.dupe(allocator);
        errdefer left.deinit(allocator);
        const right = try self.right.dupe(allocator);
        errdefer right.deinit(allocator);
        return Pair.init(allocator, left, right);
    }

    pub fn write(self: Pair, writer: anytype) !void {
        try writer.writeByte('(');
        try self.left.write(writer);
        _ = try writer.write(" . ");
        try self.right.write(writer);
        try writer.writeByte(')');
    }
};

pub const Expression = union(enum) {
    bool: bool,
    int: i64,
    float: f64,
    char: u8,
    string: []const u8,
    ident: []const u8,
    list: LinkedListWrapper,
    pair: Pair,

    pub fn deinit(self: Expression, allocator: Allocator) void {
        switch (self) {
            .string => |string| allocator.free(string),
            .ident => |ident| allocator.free(ident),
            .list => |list| list.deinit(allocator),
            .pair => |pair| pair.deinit(allocator),
            else => {},
        }
    }

    pub fn dupe(self: Expression, allocator: Allocator) Allocator.Error!Expression {
        return switch (self) {
            .bool, .int, .float, .char => self,
            .string => |string| .{ .string = try allocator.dupe(u8, string) },
            .ident => |ident| .{ .ident = try allocator.dupe(u8, ident) },
            .list => |list| .{ .list = try list.dupe(allocator) },
            .pair => |pair| .{ .pair = try pair.dupe(allocator) },
        };
    }

    pub fn write(self: Expression, writer: anytype) !void {
        return switch (self) {
            .bool => |b| std.fmt.format(writer, "{s}", .{if (b) "true" else "false"}),
            .int => |int| std.fmt.format(writer, "{d}", .{int}),
            .float => |float| std.fmt.format(writer, "{d}", .{float}),
            .char => |char| std.fmt.format(writer, "\'{c}\'", .{char}),
            .string => |string| std.fmt.format(writer, "\"{s}\"", .{string}),
            .ident => |ident| std.fmt.format(writer, "{s}", .{ident}),
            .list => |list| list.write(writer),
            .pair => |pair| pair.write(writer),
        };
    }

    pub fn bindSymbol(self: *Expression, allocator: Allocator, param_to_arg: std.StringHashMapUnmanaged(Expression)) Allocator.Error!void {
        switch (self.*) {
            .bool, .int, .float, .char, .string => {},
            .ident => |ident| {
                if (param_to_arg.get(ident)) |value| {
                    // Create a quoted version of the expression
                    var ll = LinkedListWrapper{ .value = null };
                    errdefer ll.deinit(allocator);
                    const replace_by = try value.dupe(allocator);
                    ll.prepend(allocator, replace_by) catch |err| {
                        replace_by.deinit(allocator);
                        return err;
                    };
                    const quote = Expression{ .ident = try allocator.dupe(u8, "quote") };
                    ll.prepend(allocator, quote) catch |err| {
                        quote.deinit(allocator);
                        return err;
                    };

                    self.* = Expression{ .list = ll };
                    allocator.free(ident);
                }
            },
            .list => |*list| {
                try list.bindSymbol(allocator, param_to_arg);
            },
            .pair => |*pair| {
                try pair.left.bindSymbol(allocator, param_to_arg);
                try pair.right.bindSymbol(allocator, param_to_arg);
            },
        }
    }
};

const Error = error{ UnexpectedEndOfInput, UnexpectedRparen };

const Self = @This();

allocator: Allocator,
tokenizer: Tokenizer,

pub fn init(allocator: Allocator, source: []const u8) Self {
    return .{
        .allocator = allocator,
        .tokenizer = Tokenizer.init(source),
    };
}

fn parseList(self: *Self) (Allocator.Error || Tokenizer.Error || Error)!LinkedListWrapper {
    var list: ?*LinkedList = null;
    var curr = list;
    errdefer {
        const wrapper = LinkedListWrapper{ .value = list };
        wrapper.deinit(self.allocator);
    }
    while (true) {
        const element = if (self.parseExpression()) |expression|
            expression orelse return error.UnexpectedEndOfInput
        else |err| switch (err) {
            error.UnexpectedRparen => break,
            else => |leftover_err| return leftover_err,
        };
        const new_node = try self.allocator.create(LinkedList);
        new_node.* = LinkedList{ .payload = element, .next = null };
        if (curr) |l| {
            l.next = new_node;
            curr = new_node;
        } else {
            list = new_node;
            curr = new_node;
        }
    }
    return .{ .value = list };
}

pub fn parseExpression(self: *Self) (Allocator.Error || Tokenizer.Error || Error)!?Expression {
    const maybe_token = try self.tokenizer.next(self.allocator);
    const token = maybe_token orelse return null;

    // Technically shouldn't be needed, but this is more future-proof
    errdefer token.deinit(self.allocator);

    const expression: Expression = switch (token) {
        .lparen => .{ .list = try self.parseList() },
        .rparen => return error.UnexpectedRparen,
        .bool => |b| .{ .bool = b },
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
            var iter = list.iterator();
            try expect(switch (iter.next().?.payload) {
                .string => |string| std.mem.eql(u8, string, "hello"),
                else => false,
            });
            try expect(switch (iter.next().?.payload) {
                .int => |int| int == 42,
                else => false,
            });
            try expect(switch (iter.next().?.payload) {
                .list => |inner| inner.value == null,
                else => false,
            });
            try expect(switch (iter.next().?.payload) {
                .char => |char| char == 'c',
                else => false,
            });
            try expect(switch (iter.next().?.payload) {
                .float => |float| float == 3.14,
                else => false,
            });
            try expect(iter.next() == null);
        },
        else => return error.ExpectedList,
    }
    try expect(try parser.parseExpression() == null);
}
