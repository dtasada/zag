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

pub fn init(alloc: std.mem.Allocator, name: []const u8) Self {
    return .{
        .name = name,

        .symbols = .init(alloc),
    };
}

pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
    self.symbols.deinit();
    if (self.source_buffer) |s| alloc.free(s);
}
