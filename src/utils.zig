const std = @import("std");

pub const Capture = struct {
    name: []const u8,
    takes_ref: union(enum) {
        none,
        /// is mutable
        some: bool,
    },
    index: ?[]const u8,

    pub fn clone(self: Capture, alloc: std.mem.Allocator) !Capture {
        return .{
            .name = try alloc.dupe(u8, self.name),
            .takes_ref = self.takes_ref,
            .index = if (self.index) |i| try alloc.dupe(u8, i) else null,
        };
    }

    pub fn deinit(self: Capture, alloc: std.mem.Allocator) void {
        alloc.free(self.name);
        if (self.index) |i| alloc.free(i);
    }
};

pub const Binding = enum { let_mut, let, @"const" };

pub const CompoundTypeTag = enum {
    @"struct",
    @"enum",
    @"union",
};

pub const Position = struct {
    line: usize,
    col: usize,
    path: []const u8,

    pub fn format(self: Position, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.print("{s}:{}:{}", .{ self.path, self.line, self.col });
    }

    pub fn clone(self: Position, alloc: std.mem.Allocator) !Position {
        return .{
            .line = self.line,
            .col = self.col,
            .path = try alloc.dupe(u8, self.path),
        };
    }

    pub fn deinit(self: Position, alloc: std.mem.Allocator) void {
        alloc.free(self.path);
    }
};

const Color = enum { white, red, green, blue, yellow };

var buf: [1024]u8 = undefined;
pub fn print(comptime fmt: []const u8, args: anytype, comptime color: Color) void {
    var stdout_writer = std.fs.File.stdout().writer(&buf);
    var stdout = &stdout_writer.interface;

    stdout.print(
        switch (color) {
            .white => "",
            .red => "\x1b[0;31m",
            .green => "\x1b[0;32m",
            .blue => "\x1b[0;34m",
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

var prng: std.Random.DefaultPrng = .init(0);
pub fn randInt(comptime T: type) T {
    const rand = prng.random();
    return rand.int(T);
}

pub fn cloneSlice(comptime T: type, list: []const T, alloc: std.mem.Allocator) ![]T {
    const new_list = try alloc.alloc(T, list.len);
    for (list, 0..) |item, i| new_list[i] = try item.clone(alloc);
    return new_list;
}

pub fn deinitSlice(comptime T: type, list: []const T, alloc: std.mem.Allocator) void {
    for (list) |i| i.deinit(alloc);
    alloc.free(list);
}
