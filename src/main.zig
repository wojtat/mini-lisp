const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const Expression = Parser.Expression;
const List = Parser.List;
const Allocator = std.mem.Allocator;

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
        try environment.mapSymbol(allocator, "lambda", .{ .ident = "lambda" });
        try environment.mapSymbol(allocator, "define", .{ .ident = "define" });
        try environment.mapSymbol(allocator, "quote", .{ .ident = "quote" });
        try environment.mapSymbol(allocator, "if", .{ .ident = "if" });
        try environment.mapSymbol(allocator, "car", .{ .ident = "car" });
        try environment.mapSymbol(allocator, "cdr", .{ .ident = "cdr" });
        try environment.mapSymbol(allocator, "cons", .{ .ident = "cons" });
        try environment.mapSymbol(allocator, "empty?", .{ .ident = "empty?" });
        try environment.mapSymbol(allocator, "+", .{ .ident = "+" });
        try environment.mapSymbol(allocator, "-", .{ .ident = "-" });
        try environment.mapSymbol(allocator, "*", .{ .ident = "*" });
        try environment.mapSymbol(allocator, "/", .{ .ident = "/" });
        try environment.mapSymbol(allocator, ">", .{ .ident = ">" });
        return .{
            .allocator = allocator,
            .environment = environment,
        };
    }

    fn deinit(self: *Evaluator) void {
        self.environment.deinit(self.allocator);
    }

    fn builtInLambda(self: *Evaluator, list: List) !Expression {
        // This evaluates to the procedure defined by the lambda
        // Return the original list
        return .{ .list = try list.dupe(self.allocator) };
    }

    fn builtInDefine(self: *Evaluator, list: List) !Expression {
        // TODO: Handle errors
        const id = switch (list.elements.items[1]) {
            .ident => |id_ident| id_ident,
            else => return error.ExpectedIdentifierInDefine,
        };
        const value = try self.evaluate(list.elements.items[2]);
        errdefer value.deinit(self.allocator);
        try self.environment.mapSymbol(self.allocator, id, value);
        const dummy_elements = std.ArrayList(Expression).init(self.allocator);
        return .{ .list = .{ .elements = dummy_elements } };
    }

    fn builtInQuote(self: *Evaluator, list: List) !Expression {
        if (list.elements.items.len != 2) {
            return error.WrongNumberOfArguments;
        }
        return list.elements.items[1].dupe(self.allocator);
    }

    fn builtInIf(self: *Evaluator, list: List) !Expression {
        if (list.elements.items.len != 4) {
            return error.WrongNumberOfArguments;
        }
        const condition = try self.evaluate(list.elements.items[1]);
        defer condition.deinit(self.allocator);
        const is_true = switch (condition) {
            .bool => |b| b,
            else => true,
        };
        if (is_true) {
            return self.evaluate(list.elements.items[2]);
        } else {
            return self.evaluate(list.elements.items[3]);
        }
    }

    fn builtInCar(self: *Evaluator, list: List) !Expression {
        if (list.elements.items.len != 2) {
            return error.WrongNumberOfArguments;
        }
        var argument = try self.evaluate(list.elements.items[1]);
        defer argument.deinit(self.allocator);
        const car = switch (argument) {
            .list => |*inner| blk: {
                if (inner.elements.items.len == 0) {
                    return error.ExpectedCons;
                }
                // NOTE: Swap remove is fine because the list will be thrown away anyway
                break :blk inner.elements.swapRemove(0);
            },
            // TODO: Cons cell
            else => return error.ExpectedCons,
        };
        return car;
    }

    fn builtInCdr(self: *Evaluator, list: List) !Expression {
        if (list.elements.items.len != 2) {
            return error.WrongNumberOfArguments;
        }
        var argument = try self.evaluate(list.elements.items[1]);
        errdefer argument.deinit(self.allocator);
        switch (argument) {
            .list => |*inner| {
                if (inner.elements.items.len == 0) {
                    return error.ExpectedCons;
                }
                const expr = inner.elements.orderedRemove(0);
                expr.deinit(self.allocator);
            },
            // TODO: Cons cell
            else => return error.ExpectedCons,
        }
        return argument;
    }

    fn builtInCons(self: *Evaluator, list: List) !Expression {
        _ = self;
        _ = list;
        return error.Todo;
    }

    fn builtInEmpty(self: *Evaluator, list: List) !Expression {
        if (list.elements.items.len != 2) {
            return error.WrongNumberOfArguments;
        }
        const argument = try self.evaluate(list.elements.items[1]);
        defer argument.deinit(self.allocator);
        const is_empty = switch (argument) {
            .list => |inner| inner.elements.items.len == 0,
            else => false,
        };
        return .{ .bool = is_empty };
    }

    fn builtInAdd(self: *Evaluator, list: List) !Expression {
        var int_sum: i64 = 0;
        var idx: usize = 1;
        for (list.elements.items[1..]) |summand_expression| {
            const evaluated = try self.evaluate(summand_expression);
            // NOTE: deinit for numbers is a nop, so this works
            //       even though we use the float again later
            defer evaluated.deinit(self.allocator);
            switch (evaluated) {
                .int => |int| int_sum += int,
                .float => break,
                else => return error.ExpectedNumber,
            }
            idx += 1;
        }
        if (idx == list.elements.items.len) {
            // All numbers were integers
            return .{ .int = int_sum };
        }
        // We stopped at a float
        var float_sum: f64 = @floatFromInt(int_sum);
        for (list.elements.items[idx..]) |summand_expression| {
            const evaluated = try self.evaluate(summand_expression);
            defer evaluated.deinit(self.allocator);
            switch (evaluated) {
                .int => |int| float_sum += @floatFromInt(int),
                .float => |float| float_sum += float,
                else => return error.ExpectedNumber,
            }
        }
        return .{ .float = float_sum };
    }

    fn builtInSub(self: *Evaluator, list: List) !Expression {
        if (list.elements.items.len == 1) {
            return error.WrongNumberOfArguments;
        }
        if (list.elements.items.len == 2) {
            const argument = try self.evaluate(list.elements.items[1]);
            defer argument.deinit(self.allocator);
            return switch (argument) {
                .int => |int| .{ .int = -int },
                .float => |float| .{ .float = -float },
                else => return error.ExpectedNumber,
            };
        }

        const first = try self.evaluate(list.elements.items[1]);
        defer first.deinit(self.allocator);
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
            for (list.elements.items[idx..]) |summand_expression| {
                const evaluated = try self.evaluate(summand_expression);
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

            if (idx == list.elements.items.len) {
                // All numbers were integers
                return .{ .int = int_sum };
            }
            float_sum = @floatFromInt(int_sum);
        }

        // We stopped at a float
        for (list.elements.items[idx..]) |summand_expression| {
            const evaluated = try self.evaluate(summand_expression);
            defer evaluated.deinit(self.allocator);
            switch (evaluated) {
                .int => |int| float_sum -= @floatFromInt(int),
                .float => |float| float_sum -= float,
                else => return error.ExpectedNumber,
            }
        }
        return .{ .float = float_sum };
    }

    fn builtInMul(self: *Evaluator, list: List) !Expression {
        var int_prod: i64 = 1;
        var idx: usize = 1;
        for (list.elements.items[1..]) |factor_expression| {
            const evaluated = try self.evaluate(factor_expression);
            // NOTE: deinit for numbers is a nop, so this works
            //       even though we use the float again later
            defer evaluated.deinit(self.allocator);
            switch (evaluated) {
                .int => |int| int_prod *= int,
                .float => break,
                else => return error.ExpectedNumber,
            }
            idx += 1;
        }
        if (idx == list.elements.items.len) {
            // All numbers were integers
            return .{ .int = int_prod };
        }
        // We stopped at a float
        var float_prod: f64 = @floatFromInt(int_prod);
        for (list.elements.items[idx..]) |factor_expression| {
            const evaluated = try self.evaluate(factor_expression);
            defer evaluated.deinit(self.allocator);
            switch (evaluated) {
                .int => |int| float_prod *= @floatFromInt(int),
                .float => |float| float_prod *= float,
                else => return error.ExpectedNumber,
            }
        }
        return .{ .float = float_prod };
    }

    fn builtInDiv(self: *Evaluator, list: List) !Expression {
        _ = self;
        _ = list;
        return error.Todo;
    }

    fn builtInGreater(self: *Evaluator, list: List) !Expression {
        _ = self;
        _ = list;
        return error.Todo;
    }

    fn call(self: *Evaluator, list: List) !Expression {
        if (list.elements.items.len == 0) {
            return error.MissingProcedure;
        }
        // Obtain the lambda function to be called
        const callee = try self.evaluate(list.elements.items[0]);
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
                } else {
                    return error.ExpectedCallable;
                }
            },
            .list => |lambda_list| {
                // Should be a lambda list
                // Check the basic form of the lambda
                // Lambda is a list with at least three elements
                if (lambda_list.elements.items.len < 3) {
                    return error.ExpectedCallable;
                }
                // First element is keyword "lambda"
                const isLambda = switch (lambda_list.elements.items[0]) {
                    .ident => |ident| std.mem.eql(u8, ident, "lambda"),
                    else => false,
                };
                if (!isLambda) {
                    return error.ExpectedCallable;
                }
            },
        }
        const parameter_list = switch (callee.list.elements.items[1]) {
            .list => |param_list| param_list,
            else => return error.MalformedLambda,
        };
        // TODO: Default arguments?
        if (parameter_list.elements.items.len + 1 != list.elements.items.len) {
            return error.WrongNumberOfArguments;
        }
        // Populate environment
        try self.environment.pushFrame(self.allocator);
        for (list.elements.items[1..], parameter_list.elements.items) |argument, parameter| {
            // Evaluate function arguments eagerly
            const argument_evaluated = try self.evaluate(argument);
            errdefer argument_evaluated.deinit(self.allocator);
            const parameter_ident = switch (parameter) {
                .ident => |param_ident| param_ident,
                else => return error.MalformedLambda,
            };
            try self.environment.mapSymbol(self.allocator, parameter_ident, argument_evaluated);
        }
        // Evaluate the body
        var maybe_return_expression: ?Expression = null;
        const num_body_expressions = callee.list.elements.items.len - 2;
        for (callee.list.elements.items[2..], 0..) |body_expression, idx| {
            const expr = try self.evaluate(body_expression);
            if (idx + 1 == num_body_expressions) {
                maybe_return_expression = expr;
            } else {
                expr.deinit(self.allocator);
            }
        }
        const return_expression = maybe_return_expression orelse unreachable;
        errdefer return_expression.deinit(self.allocator);
        try self.environment.popAndDeinitFrame(self.allocator);
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
        \\((lambda (op n)
        \\    (define five 5)
        \\    (+ 1.7 five n (op 4))) (quote +) 3.3)
    ;

    var parser = Parser.init(allocator, source);
    const expression = (try parser.parseExpression()) orelse return error.UnexpectedEndOfInput;
    defer expression.deinit(allocator);
    expression.debugPrint();
    std.debug.print("\n", .{});

    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();
    const evaluated = try evaluator.evaluate(expression);
    defer evaluated.deinit(allocator);
    evaluated.debugPrint();
    std.debug.print("\n", .{});
}
