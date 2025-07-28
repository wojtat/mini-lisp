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

    fn dupe(self: List, allocator: std.mem.Allocator) !List {
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
    string: []const u8,
    ident: []const u8,
    list: List,

    fn deinit(self: Expression, allocator: std.mem.Allocator) void {
        switch (self) {
            .string => |string| allocator.free(string),
            .ident => |ident| allocator.free(ident),
            .list => |list| list.deinit(allocator),
            else => {},
        }
    }

    fn dupe(self: Expression, allocator: std.mem.Allocator) anyerror!Expression {
        return switch (self) {
            .int, .float, .char => self,
            .string => |string| .{ .string = try allocator.dupe(u8, string) },
            .ident => |ident| .{ .ident = try allocator.dupe(u8, ident) },
            .list => |list| .{ .list = try list.dupe(allocator) },
        };
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

const VariableEnvironment = struct {
    const SymbolValue = struct {
        symbol: []const u8,
        value: ?Expression,
    };

    symbols: std.StringHashMapUnmanaged(Expression) = .{},
    symbol_stack: std.ArrayListUnmanaged(SymbolValue) = .{},
    stack_counts: std.ArrayListUnmanaged(usize) = .{},

    fn deinit(self: *VariableEnvironment, allocator: std.mem.Allocator) void {
        self.symbols.deinit(allocator);
        self.symbol_stack.deinit(allocator);
        self.stack_counts.deinit(allocator);
    }

    fn pushFrame(self: *VariableEnvironment, allocator: std.mem.Allocator) !void {
        try self.stack_counts.append(allocator, 0);
    }

    fn popAndDeinitFrame(self: *VariableEnvironment, allocator: std.mem.Allocator) !void {
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

    fn mapSymbol(self: *VariableEnvironment, allocator: std.mem.Allocator, symbol: []const u8, value: Expression) !void {
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
    allocator: std.mem.Allocator,
    environment: VariableEnvironment,

    fn init(allocator: std.mem.Allocator) !Evaluator {
        var environment = VariableEnvironment{};
        errdefer environment.deinit(allocator);
        try environment.pushFrame(allocator);
        // TODO: Add the rest of the built-ins
        try environment.mapSymbol(allocator, "lambda", .{ .ident = "lambda" });
        try environment.mapSymbol(allocator, "define", .{ .ident = "define" });
        try environment.mapSymbol(allocator, "if", .{ .ident = "if" });
        return .{
            .allocator = allocator,
            .environment = environment,
        };
    }

    fn deinit(self: *Evaluator) void {
        self.environment.deinit(self.allocator);
    }

    fn call(self: *Evaluator, list: List) !Expression {
        if (list.elements.items.len == 0) {
            return error.MissingProcedure;
        }
        // Obtain the lambda function to be called
        const callee = try self.evaluate(list.elements.items[0]);
        defer callee.deinit(self.allocator);
        switch (callee) {
            .char, .int, .float, .string => return error.ExpectedCallable,
            .ident => |ident| {
                if (std.mem.eql(u8, ident, "lambda")) {
                    // This evaluates to the procedure defined by the lambda
                    // Return the original list
                    return .{
                        .list = try list.dupe(self.allocator),
                    };
                } else if (std.mem.eql(u8, ident, "define")) {
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
                } else if (std.mem.eql(u8, ident, "if")) {
                    // TODO: Handle errors
                    const condition = try self.evaluate(list.elements.items[1]);
                    defer condition.deinit(self.allocator);
                    const is_true = switch (condition) {
                        .list => |cond_list| cond_list.elements.items.len == 0,
                        else => true,
                    };
                    if (is_true) {
                        return self.evaluate(list.elements.items[1]);
                    } else {
                        return self.evaluate(list.elements.items[2]);
                    }
                } else if (std.mem.eql(u8, ident, "zero?")) {
                    // TODO: Handle errors
                    const argument = try self.evaluate(list.elements.items[1]);
                    defer argument.deinit(self.allocator);
                    return error.Todo;
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
            .char, .float, .int, .string => try expression.dupe(self.allocator),
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
        \\((lambda ()
        \\    (define hi "hello")
        \\    hi))
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
