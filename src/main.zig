const std = @import("std");
const Allocator = std.mem.Allocator;
const Parser = @import("Parser.zig");
const Evaluator = @import("Evaluator.zig");

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
