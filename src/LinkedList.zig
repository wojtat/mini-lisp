const std = @import("std");
const Allocator = std.mem.Allocator;
const Parser = @import("Parser.zig");
const Expression = Parser.Expression;

const Self = @This();

payload: Expression,
next: ?*Self,

pub const Wrapper = struct {
    value: ?*Self,

    pub fn deinit(self: Wrapper, allocator: Allocator) void {
        var iter = self.iterator();
        while (iter.next()) |next| {
            next.payload.deinit(allocator);
            allocator.destroy(next);
        }
    }

    pub fn dupe(self: Wrapper, allocator: Allocator) Allocator.Error!Wrapper {
        var list: ?*Self = null;
        var curr = list;
        errdefer {
            const wrapper = Wrapper{ .value = list };
            wrapper.deinit(allocator);
        }
        var iter = self.iterator();
        while (iter.next()) |next| {
            const element = try next.payload.dupe(allocator);
            errdefer element.deinit(allocator);
            const new_node = try allocator.create(Self);
            new_node.* = Self{ .payload = element, .next = null };
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

    pub fn debugPrint(self: Wrapper) void {
        std.debug.print("(", .{});
        var iter = self.iterator();
        while (iter.next()) |next| {
            next.payload.debugPrint();
        }
        std.debug.print(") ", .{});
    }

    pub fn iterator(self: Wrapper) Iterator {
        return .{ .curr = self.value };
    }

    pub fn removeFirst(self: *Wrapper, allocator: Allocator) ?Expression {
        const first = self.value orelse return null;
        defer allocator.destroy(first);
        self.value = first.next;
        return first.payload;
    }
};

pub const Iterator = struct {
    const Item = *Self;

    curr: ?*Self,

    pub fn peek(self: Iterator) ?*Self {
        return self.curr;
    }

    pub fn next(self: *Iterator) ?*Self {
        defer if (self.curr) |curr| {
            self.curr = curr.next;
        };
        return self.curr;
    }

    pub fn len(self: *Iterator) usize {
        // Reset when we are finished
        const start = self.curr;
        defer self.curr = start;
        var size: usize = 0;
        while (self.next()) |_| {
            size += 1;
        }
        return size;
    }

    pub fn hasLen(self: *Iterator, n: usize) bool {
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

    pub fn hasLenAtLeast(self: *Iterator, n: usize) bool {
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

    pub fn advance(self: *Iterator, n: usize) void {
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
