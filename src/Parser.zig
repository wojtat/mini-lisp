const std = @import("std");
const Allocator = std.mem.Allocator;
const Tokenizer = @import("Tokenizer.zig");

const LinkedList = struct {
    payload: Expression,
    next: ?*LinkedList,
};

pub const LinkedListWrapper = struct {
    value: ?*LinkedList,

    pub fn deinit(self: LinkedListWrapper, allocator: Allocator) void {
        var iter = self.iterator();
        while (iter.next()) |next| {
            next.payload.deinit(allocator);
            allocator.destroy(next);
        }
    }

    pub fn dupe(self: LinkedListWrapper, allocator: Allocator) Allocator.Error!LinkedListWrapper {
        var list: ?*LinkedList = null;
        var curr = list;
        errdefer {
            const wrapper = LinkedListWrapper{ .value = list };
            wrapper.deinit(allocator);
        }
        var iter = self.iterator();
        while (iter.next()) |next| {
            const element = try next.payload.dupe(allocator);
            errdefer element.deinit(allocator);
            const new_node = try allocator.create(LinkedList);
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

    pub fn debugPrint(self: LinkedListWrapper) void {
        std.debug.print("(", .{});
        var iter = self.iterator();
        while (iter.next()) |next| {
            next.payload.debugPrint();
        }
        std.debug.print(") ", .{});
    }

    pub fn iterator(self: LinkedListWrapper) LinkedListIterator {
        return .{ .curr = self.value };
    }

    pub fn removeFirst(self: *LinkedListWrapper, allocator: Allocator) ?Expression {
        const first = self.value orelse return null;
        defer allocator.destroy(first);
        self.value = first.next;
        return first.payload;
    }
};

pub const LinkedListIterator = struct {
    const Item = *LinkedList;

    curr: ?*LinkedList,

    pub fn peek(self: LinkedListIterator) ?*LinkedList {
        return self.curr;
    }

    pub fn next(self: *LinkedListIterator) ?*LinkedList {
        defer if (self.curr) |curr| {
            self.curr = curr.next;
        };
        return self.curr;
    }

    pub fn len(self: *LinkedListIterator) usize {
        // Reset when we are finished
        const start = self.curr;
        defer self.curr = start;
        var size: usize = 0;
        while (self.next()) |_| {
            size += 1;
        }
        return size;
    }

    pub fn hasLen(self: *LinkedListIterator, n: usize) bool {
        if (n == 0) {
            // Edge case
            return self.curr == null;
        }
        // Reset when we are finished
        const start = self.curr;
        defer self.curr = start;
        var size: usize = 0;
        while (self.next()) |_| {
            size += 1;
            if (size == n) {
                break;
            }
        }
        return self.curr == null;
    }

    pub fn hasLenAtLeast(self: *LinkedListIterator, n: usize) bool {
        if (n == 0) {
            // Degenerate edge case
            return true;
        }
        // Reset when we are finished
        const start = self.curr;
        defer self.curr = start;
        var size: usize = 0;
        while (self.next()) |_| {
            size += 1;
            if (size >= n) {
                return true;
            }
        }
        return false;
    }

    pub fn advance(self: *LinkedListIterator, n: usize) void {
        for (0..n) |_| {
            _ = self.next();
        }
    }
};

pub fn PairIter(comptime Iter1: type, comptime Iter2: type) type {
    const Pair = struct {
        @"0": Iter1.Item,
        @"1": Iter2.Item,
    };
    const E = error{
        LeftIteratorExhausted,
        RightIteratorExhausted,
    };
    return struct {
        const I = @This();

        iter1: Iter1,
        iter2: Iter2,

        pub fn next(self: *I) E!?Pair {
            const maybe_next1 = self.iter1.next();
            const maybe_next2 = self.iter2.next();
            if (maybe_next1) |next1| {
                if (maybe_next2) |next2| {
                    return .{ .@"0" = next1, .@"1" = next2 };
                } else {
                    return E.RightIteratorExhausted;
                }
            } else {
                if (maybe_next2) |_| {
                    return E.LeftIteratorExhausted;
                } else {
                    return null;
                }
            }
        }
    };
}

pub const Expression = union(enum) {
    bool: bool,
    int: i64,
    float: f64,
    char: u8,
    string: []const u8,
    ident: []const u8,
    list: LinkedListWrapper,

    pub fn deinit(self: Expression, allocator: Allocator) void {
        switch (self) {
            .string => |string| allocator.free(string),
            .ident => |ident| allocator.free(ident),
            .list => |list| list.deinit(allocator),
            else => {},
        }
    }

    pub fn dupe(self: Expression, allocator: Allocator) Allocator.Error!Expression {
        return switch (self) {
            .bool, .int, .float, .char => self,
            .string => |string| .{ .string = try allocator.dupe(u8, string) },
            .ident => |ident| .{ .ident = try allocator.dupe(u8, ident) },
            .list => |list| .{ .list = try list.dupe(allocator) },
        };
    }

    pub fn debugPrint(self: Expression) void {
        switch (self) {
            .bool => |b| std.debug.print("{s} ", .{if (b) "true" else "false"}),
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
