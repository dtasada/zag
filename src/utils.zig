const std = @import("std");

pub fn print(
    comptime fmt: []const u8,
    args: anytype,
    comptime color: enum { white, red, green, blue, yellow },
) void {
    var buf: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&buf);
    var stdout = &stdout_writer.interface;

    stdout.print(
        switch (color) {
            .white => "",
            .red => "\x1b[0;31m",
            .green => "\x1b[0;34m",
            .blue => "\x1b[0;32m",
            .yellow => "\x1b[0;33m",
        } ++ fmt ++ "\x1b[0m",
        args,
    ) catch |err| std.debug.print("Couldn't stdout.print(): {}\n", .{err});

    stdout.flush() catch |err| std.debug.print("Couldn't stdout.flush(): {}\n", .{err});
}
