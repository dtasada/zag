const std = @import("std");

const Type = @import("Type.zig").Type;

const Self = @This();

const File = @import("Compiler.zig").File;

pub const Symbol = struct {
    is_pub: bool,
    name: []const u8,
    type: Type,
    is_mut: bool = false,
};

name: []const u8,

symbols: *std.StringHashMap(Symbol),

file: ?*File,

pub fn init(alloc: std.mem.Allocator, name: []const u8, file: ?*File) !Self {
    const symbols = try alloc.create(std.StringHashMap(Symbol));
    symbols.* = .init(alloc);

    return .{
        .name = name,
        .symbols = symbols,
        .file = file,
    };
}

pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
    self.symbols.deinit();
    if (self.source_buffer) |s| alloc.free(s);
    alloc.free(self.source_path);
}
