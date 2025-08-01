const std = @import("std");
const Allocator = std.mem.Allocator;
const Parser = @import("Parser.zig");
const Evaluator = @import("Evaluator.zig");

fn readEntireFile(allocator: Allocator, relative_path: [:0]const u8) ![]u8 {
    var file = try std.fs.cwd().openFile(relative_path, .{});
    defer file.close();
    const file_size = try file.getEndPos();
    const buffer = try allocator.alloc(u8, file_size);
    errdefer allocator.free(buffer);
    _ = try file.read(buffer);
    return buffer;
}

fn parseAndEvaluateAll(allocator: Allocator, evaluator: *Evaluator, source: []const u8, writer: anytype) !void {
    var parser = Parser.init(allocator, source);
    while (try parser.parseExpression()) |expression| {
        defer expression.deinit(allocator);
        _ = try writer.write("Expression: ");
        try expression.write(writer);
        try writer.writeByte('\n');

        const evaluated = try evaluator.evaluate(expression);
        defer evaluated.deinit(allocator);
        _ = try writer.write("Evaluates to: ");
        try evaluated.write(writer);
        try writer.writeByte('\n');
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const status = gpa.deinit();
        _ = status;
    }

    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    const stdout = std.io.getStdOut().writer();

    var args_iter = std.process.args();
    _ = args_iter.skip();
    while (args_iter.next()) |arg| {
        const contents = try readEntireFile(allocator, arg);
        defer allocator.free(contents);

        try parseAndEvaluateAll(allocator, &evaluator, contents, stdout);
    }
}
