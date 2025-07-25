const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const status = gpa.deinit();
        _ = status;
    }

    const source =
        \\(function factorial
        \\    ((int n))
        \\    int
        \\    (
        \\        (return (if (== n 0) 1 (* n (factorial (- n 1)))))
        \\    )
        \\)
        \\
        \\(function main
        \\    ((int argc) ((* (* char)) argv))
        \\    int
        \\    (
        \\        (print (append "Hello, world" '!'))
        \\        (return (factorial 10))
        \\    )
        \\)
        \\
    ;

    var tokenizer = Tokenizer.init(source);

    while (try tokenizer.next(allocator)) |*token| {
        std.debug.print("Token: {any}\n", .{token});
        token.deinit(allocator);
    }
}
