const std = @import("std");
const Allocator = std.mem.Allocator;
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const Expression = Parser.Expression;
const LinkedList = @import("LinkedList.zig");
const LinkedListWrapper = LinkedList.Wrapper;
const PairIter = LinkedList.PairIter;
const LinkedListIterator = LinkedList.Iterator;

const VariableEnvironment = struct {
    const SymbolValue = struct {
        symbol: []const u8,
        value: ?Expression,
    };

    symbols: std.StringHashMapUnmanaged(Expression) = .{},
    symbol_stack: std.ArrayListUnmanaged(SymbolValue) = .{},
    stack_counts: std.ArrayListUnmanaged(usize) = .{},

    fn deinit(self: *VariableEnvironment, allocator: Allocator) void {
        self.symbols.deinit(allocator);
        self.symbol_stack.deinit(allocator);
        self.stack_counts.deinit(allocator);
    }

    fn pushFrame(self: *VariableEnvironment, allocator: Allocator) !void {
        try self.stack_counts.append(allocator, 0);
    }

    fn popAndDeinitFrame(self: *VariableEnvironment, allocator: Allocator) !void {
        const num_symbols = self.stack_counts.popOrNull() orelse return error.InvalidOperation;
        for (0..num_symbols) |_| {
            const symbol_value = self.symbol_stack.pop();
            defer allocator.free(symbol_value.symbol);
            if (symbol_value.value) |value| {
                const maybe_kv = self.symbols.fetchPut(allocator, symbol_value.symbol, value) catch unreachable;
                const v = maybe_kv.?.value;
                v.deinit(allocator);
            } else {
                const kv = self.symbols.fetchRemove(symbol_value.symbol).?;
                kv.value.deinit(allocator);
            }
        }
    }

    fn mapSymbol(self: *VariableEnvironment, allocator: Allocator, symbol: []const u8, value: Expression) !void {
        // Must have a frame
        if (self.stack_counts.items.len == 0) {
            return error.InvalidOperation;
        }
        // Put the mapping in the table, saving the previous value of the symbol
        var previous_value: ?Expression = null;
        if (try self.symbols.fetchPut(allocator, symbol, value)) |previous| {
            previous_value = previous.value;
        }
        const symbol_value = .{ .symbol = symbol, .value = previous_value };
        try self.symbol_stack.append(allocator, symbol_value);
        // Increment the number of mapped symbols in the frame
        self.stack_counts.items[self.stack_counts.items.len - 1] += 1;
    }
};

const Evaluator = struct {
    allocator: Allocator,
    environment: VariableEnvironment,

    fn init(allocator: Allocator) !Evaluator {
        var environment = VariableEnvironment{};
        errdefer environment.deinit(allocator);
        try environment.pushFrame(allocator);

        // TODO: Add the rest of the built-ins
        const built_ins = [_][]const u8{
            "lambda",
            "define",
            "quote",
            "if",
            "car",
            "cdr",
            "cons",
            "empty?",
            "zero?",
            "+",
            "-",
            "*",
            "/",
            ">",
        };
        for (built_ins) |built_in| {
            try environment.mapSymbol(allocator, built_in, .{ .ident = built_in });
        }

        // Push a new global frame, so that we can later clean it up
        try environment.pushFrame(allocator);
        return .{
            .allocator = allocator,
            .environment = environment,
        };
    }

    fn deinit(self: *Evaluator) void {
        // We pushed a new global frame in init
        self.environment.popAndDeinitFrame(self.allocator) catch unreachable;
        self.environment.deinit(self.allocator);
    }

    fn builtInLambda(self: *Evaluator, list: LinkedListWrapper) !Expression {
        // This evaluates to the procedure defined by the lambda
        // Return the original LinkedListWrapper
        return .{ .list = try list.dupe(self.allocator) };
    }

    fn builtInDefine(self: *Evaluator, list: LinkedListWrapper) !Expression {
        var iter = list.iterator();
        iter.advance(1);
        if (!iter.hasLen(2)) {
            return error.WrongNumberOfArguments;
        }
        const id = switch (iter.next().?.payload) {
            .ident => |id_ident| id_ident,
            else => return error.ExpectedIdentifierInDefine,
        };
        const value = try self.evaluate(iter.next().?.payload);
        errdefer value.deinit(self.allocator);
        try self.environment.mapSymbol(self.allocator, try self.allocator.dupe(u8, id), value);
        return .{ .list = .{ .value = null } };
    }

    fn builtInQuote(self: *Evaluator, list: LinkedListWrapper) !Expression {
        var iter = list.iterator();
        iter.advance(1);
        if (!iter.hasLen(1)) {
            return error.WrongNumberOfArguments;
        }
        return iter.next().?.payload.dupe(self.allocator);
    }

    fn builtInIf(self: *Evaluator, list: LinkedListWrapper) !Expression {
        var iter = list.iterator();
        iter.advance(1);
        if (!iter.hasLen(3)) {
            return error.WrongNumberOfArguments;
        }
        const condition = try self.evaluate(iter.next().?.payload);
        defer condition.deinit(self.allocator);
        const is_true = switch (condition) {
            .bool => |b| b,
            else => true,
        };
        if (is_true) {
            // Skip
        } else {
            iter.advance(1);
        }
        return self.evaluate(iter.next().?.payload);
    }

    fn builtInCar(self: *Evaluator, list: LinkedListWrapper) !Expression {
        var iter = list.iterator();
        iter.advance(1);
        if (!iter.hasLen(1)) {
            return error.WrongNumberOfArguments;
        }
        var argument = try self.evaluate(iter.next().?.payload);
        defer argument.deinit(self.allocator);
        const car = switch (argument) {
            .list => |*inner| blk: {
                const value = inner.removeFirst(self.allocator) orelse return error.ExpectedCons;
                break :blk value;
            },
            else => return error.ExpectedCons,
        };
        return car;
    }

    fn builtInCdr(self: *Evaluator, list: LinkedListWrapper) !Expression {
        var iter = list.iterator();
        iter.advance(1);
        if (!iter.hasLen(1)) {
            return error.WrongNumberOfArguments;
        }
        var argument = try self.evaluate(iter.next().?.payload);
        errdefer argument.deinit(self.allocator);
        switch (argument) {
            .list => |*inner| {
                if (inner.removeFirst(self.allocator) == null) {
                    return error.ExpectedCons;
                }
            },
            else => return error.ExpectedCons,
        }
        return argument;
    }

    fn builtInCons(self: *Evaluator, list: LinkedListWrapper) !Expression {
        _ = self;
        _ = list;
        return error.Todo;
    }

    fn builtInEmpty(self: *Evaluator, list: LinkedListWrapper) !Expression {
        var iter = list.iterator();
        iter.advance(1);
        if (!iter.hasLen(1)) {
            return error.WrongNumberOfArguments;
        }
        var argument = try self.evaluate(iter.next().?.payload);
        defer argument.deinit(self.allocator);
        const is_empty = switch (argument) {
            .list => |inner| inner.value == null,
            else => false,
        };
        return .{ .bool = is_empty };
    }

    fn builtInAdd(self: *Evaluator, list: LinkedListWrapper) !Expression {
        var iter = list.iterator();
        iter.advance(1);
        var int_sum: i64 = 0;
        while (iter.peek()) |next| : (_ = iter.next()) {
            const evaluated = try self.evaluate(next.payload);
            // NOTE: deinit for numbers is a nop, so this works
            //       even though we use the float again later
            defer evaluated.deinit(self.allocator);
            switch (evaluated) {
                .int => |int| int_sum += int,
                .float => break,
                else => return error.ExpectedNumber,
            }
        }
        if (iter.hasLen(0)) {
            // All numbers were integers
            return .{ .int = int_sum };
        }
        // We stopped at a float
        var float_sum: f64 = @floatFromInt(int_sum);
        while (iter.next()) |next| {
            const evaluated = try self.evaluate(next.payload);
            defer evaluated.deinit(self.allocator);
            switch (evaluated) {
                .int => |int| float_sum += @floatFromInt(int),
                .float => |float| float_sum += float,
                else => return error.ExpectedNumber,
            }
        }
        return .{ .float = float_sum };
    }

    fn builtInSub(self: *Evaluator, list: LinkedListWrapper) !Expression {
        var iter = list.iterator();
        iter.advance(1);
        if (!iter.hasLenAtLeast(1)) {
            return error.WrongNumberOfArguments;
        }

        const first = try self.evaluate(iter.next().?.payload);
        defer first.deinit(self.allocator);
        if (iter.hasLen(0)) {
            // Unary minus
            return switch (first) {
                .int => |int| .{ .int = -int },
                .float => |float| .{ .float = -float },
                else => return error.ExpectedNumber,
            };
        }

        var int_sum: i64 = 0;
        var float_sum: f64 = 0.0;
        const is_int = switch (first) {
            .int => |int| blk: {
                int_sum = int;
                break :blk true;
            },
            .float => |float| blk: {
                float_sum = float;
                break :blk false;
            },
            else => return error.ExpectedNumber,
        };
        var idx: usize = 2;

        if (is_int) {
            while (iter.peek()) |next| : (_ = iter.next()) {
                const evaluated = try self.evaluate(next.payload);
                // NOTE: deinit for numbers is a nop, so this works
                //       even though we use the float again later
                defer evaluated.deinit(self.allocator);
                switch (evaluated) {
                    .int => |int| int_sum -= int,
                    .float => break,
                    else => return error.ExpectedNumber,
                }
                idx += 1;
            }

            if (iter.hasLen(0)) {
                // All numbers were integers
                return .{ .int = int_sum };
            }
            float_sum = @floatFromInt(int_sum);
        }

        // We stopped at a float
        while (iter.next()) |next| {
            const evaluated = try self.evaluate(next.payload);
            defer evaluated.deinit(self.allocator);
            switch (evaluated) {
                .int => |int| float_sum -= @floatFromInt(int),
                .float => |float| float_sum -= float,
                else => return error.ExpectedNumber,
            }
        }
        return .{ .float = float_sum };
    }

    fn builtInMul(self: *Evaluator, list: LinkedListWrapper) !Expression {
        var iter = list.iterator();
        iter.advance(1);
        var int_prod: i64 = 1;
        while (iter.peek()) |next| : (_ = iter.next()) {
            const evaluated = try self.evaluate(next.payload);
            // NOTE: deinit for numbers is a nop, so this works
            //       even though we use the float again later
            defer evaluated.deinit(self.allocator);
            switch (evaluated) {
                .int => |int| int_prod *= int,
                .float => break,
                else => return error.ExpectedNumber,
            }
        }
        if (iter.hasLen(0)) {
            // All numbers were integers
            return .{ .int = int_prod };
        }
        // We stopped at a float
        var float_prod: f64 = @floatFromInt(int_prod);
        while (iter.next()) |next| {
            const evaluated = try self.evaluate(next.payload);
            defer evaluated.deinit(self.allocator);
            switch (evaluated) {
                .int => |int| float_prod *= @floatFromInt(int),
                .float => |float| float_prod *= float,
                else => return error.ExpectedNumber,
            }
        }
        return .{ .float = float_prod };
    }

    fn builtInDiv(self: *Evaluator, list: LinkedListWrapper) !Expression {
        _ = self;
        _ = list;
        return error.Todo;
    }

    fn builtInGreater(self: *Evaluator, list: LinkedListWrapper) !Expression {
        _ = self;
        _ = list;
        return error.Todo;
    }

    fn builtInZero(self: *Evaluator, list: LinkedListWrapper) !Expression {
        var iter = list.iterator();
        iter.advance(1);
        if (!iter.hasLen(1)) {
            return error.WrongNumberOfArguments;
        }
        var argument = try self.evaluate(iter.next().?.payload);
        defer argument.deinit(self.allocator);
        const is_zero = switch (argument) {
            .int => |int| int == 0,
            .float => |float| float == 0,
            else => false,
        };
        return .{ .bool = is_zero };
    }

    fn call(self: *Evaluator, list: LinkedListWrapper) !Expression {
        var iter = list.iterator();
        // Obtain the lambda function to be called
        const callee = blk: {
            const next = iter.next() orelse return error.MissingProcedure;
            break :blk try self.evaluate(next.payload);
        };
        defer callee.deinit(self.allocator);
        switch (callee) {
            .bool, .char, .int, .float, .string => return error.ExpectedCallable,
            .ident => |ident| {
                if (std.mem.eql(u8, ident, "lambda")) {
                    return self.builtInLambda(list);
                } else if (std.mem.eql(u8, ident, "define")) {
                    return self.builtInDefine(list);
                } else if (std.mem.eql(u8, ident, "quote")) {
                    return self.builtInQuote(list);
                } else if (std.mem.eql(u8, ident, "if")) {
                    return self.builtInIf(list);
                } else if (std.mem.eql(u8, ident, "car")) {
                    return self.builtInCar(list);
                } else if (std.mem.eql(u8, ident, "cdr")) {
                    return self.builtInCdr(list);
                } else if (std.mem.eql(u8, ident, "cons")) {
                    return self.builtInCons(list);
                } else if (std.mem.eql(u8, ident, "empty?")) {
                    return self.builtInEmpty(list);
                } else if (std.mem.eql(u8, ident, "+")) {
                    return self.builtInAdd(list);
                } else if (std.mem.eql(u8, ident, "-")) {
                    return self.builtInSub(list);
                } else if (std.mem.eql(u8, ident, "*")) {
                    return self.builtInMul(list);
                } else if (std.mem.eql(u8, ident, "/")) {
                    return self.builtInDiv(list);
                } else if (std.mem.eql(u8, ident, ">")) {
                    return self.builtInGreater(list);
                } else if (std.mem.eql(u8, ident, "zero?")) {
                    return self.builtInZero(list);
                } else {
                    return error.ExpectedCallable;
                }
            },
            .list => |lambda_list| {
                // Should be a lambda list
                // Check the basic form of the lambda
                var lambda_iter = lambda_list.iterator();
                // Lambda is a list with at least three elements
                if (!lambda_iter.hasLenAtLeast(3)) {
                    return error.ExpectedCallable;
                }
                // First element is keyword "lambda"
                const is_lambda = switch (lambda_iter.next().?.payload) {
                    .ident => |ident| std.mem.eql(u8, ident, "lambda"),
                    else => false,
                };
                if (!is_lambda) {
                    return error.ExpectedCallable;
                }
            },
        }
        var callee_iter = callee.list.iterator();
        callee_iter.advance(1);
        const parameter_list = switch (callee_iter.next().?.payload) {
            .list => |param_list| param_list,
            else => return error.MalformedLambda,
        };
        // TODO: Default arguments?

        // Populate environment
        try self.environment.pushFrame(self.allocator);
        // We must be in a valid state since we just pushed a frame
        defer self.environment.popAndDeinitFrame(self.allocator) catch unreachable;

        var pair_iter = PairIter(LinkedListIterator, LinkedListIterator){ .iter1 = iter, .iter2 = parameter_list.iterator() };
        while (pair_iter.next() catch return error.WrongNumberOfArguments) |next| {
            const argument = try self.evaluate(next.@"0".payload);
            errdefer argument.deinit(self.allocator);
            const parameter_ident = switch (next.@"1".payload) {
                .ident => |param_ident| param_ident,
                else => return error.MalformedLambda,
            };
            try self.environment.mapSymbol(self.allocator, try self.allocator.dupe(u8, parameter_ident), argument);
        }
        // Evaluate the body
        var maybe_return_expression: ?Expression = null;
        while (callee_iter.next()) |next| {
            const expr = try self.evaluate(next.payload);
            if (callee_iter.hasLenAtLeast(1)) {
                expr.deinit(self.allocator);
            } else {
                maybe_return_expression = expr;
            }
        }
        const return_expression = maybe_return_expression orelse unreachable;
        errdefer return_expression.deinit(self.allocator);
        return return_expression;
    }

    fn evaluate(self: *Evaluator, expression: Expression) anyerror!Expression {
        const evaluated = switch (expression) {
            .bool, .char, .float, .int, .string => try expression.dupe(self.allocator),
            .ident => |ident| blk: {
                const maybe_value = self.environment.symbols.get(ident);
                if (maybe_value) |value| {
                    break :blk value.dupe(self.allocator);
                } else {
                    std.debug.print("Unbound: {s}\n", .{ident});
                    return error.UnboundSymbol;
                }
            },
            .list => |list| blk: {
                break :blk self.call(list);
            },
        };
        return evaluated;
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
        \\    (if (zero? n)
        \\        1
        \\        (* n (factorial (- n 1))))))
        \\
        \\(define fact factorial)
        \\
        \\(define f10 (fact 10))
        \\
        \\f10
    ;

    var parser = Parser.init(allocator, source);
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    while (try parser.parseExpression()) |expression| {
        defer expression.deinit(allocator);
        std.debug.print("Expression: ", .{});
        expression.debugPrint();
        std.debug.print("\n", .{});

        const evaluated = try evaluator.evaluate(expression);
        defer evaluated.deinit(allocator);
        std.debug.print("Evaluates to: ", .{});
        evaluated.debugPrint();
        std.debug.print("\n", .{});
    }
}
