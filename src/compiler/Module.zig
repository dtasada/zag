const std = @import("std");

const Type = @import("Type.zig").Type;

const Self = @This();

pub const Symbol = struct {
    is_pub: bool,
    name: []const u8,
    type: Type,
};

name: []const u8,

symbols: std.StringHashMap(Symbol),

source_buffer: ?[]u8 = null,
source_path: []const u8,

pub fn init(alloc: std.mem.Allocator, name: []const u8, source_path: []const u8) Self {
    return .{
        .name = name,
        .source_path = source_path,
        .symbols = .init(alloc),
    };
}

pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
    self.symbols.deinit();
    if (self.source_buffer) |s| alloc.free(s);
    alloc.free(self.source_path);
}
