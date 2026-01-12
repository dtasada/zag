const std = @import("std");

pub const Position = struct {
    line: usize,
    col: usize,
    file_path: ?[]const u8 = null,

    pub fn format(self: Position, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        if (self.file_path) |path| {
            try writer.print("{s}:{}:{}", .{ path, self.line, self.col });
        } else {
            try writer.print("{}:{}", .{ self.line, self.col });
        }
    }
};

const Color = enum { white, red, green, blue, yellow };

pub fn print(comptime fmt: []const u8, args: anytype, comptime color: Color) void {
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

pub inline fn printErr(
    comptime err: anytype,
    comptime fmt: []const u8,
    args: anytype,
    comptime color: Color,
) @TypeOf(err) {
    print(fmt, args, color);
    return err;
}

pub fn randInt(comptime T: type) T {
    var prng: std.Random.DefaultPrng = .init(0);
    const rand = prng.random();
    return rand.int(T);
}
