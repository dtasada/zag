const std = @import("std");

const Type = @import("Type.zig").Type;

const Self = @This();

pub const Symbol = struct {
    is_pub: bool,
    name: []const u8,
    inner_name: []const u8,
    type: Type,
    is_mut: bool = false,
};

name: []const u8,

symbols: *std.StringHashMap(Symbol),

pub fn init(alloc: std.mem.Allocator, name: []const u8) !Self {
    const symbols = try alloc.create(std.StringHashMap(Symbol));
    symbols.* = .init(alloc);

    return .{
        .name = name,
        .symbols = symbols,
    };
}

pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
    self.symbols.deinit();
    if (self.source_buffer) |s| alloc.free(s);
    alloc.free(self.source_path);
}
