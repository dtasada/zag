const std = @import("std");

const Type = @import("Type.zig").Type;

const Self = @This();

pub const Symbol = struct {
    is_pub: bool,
    name: []const u8,
    inner_name: []const u8,
    type: Type,
    is_mut: bool = false,

    fn deinit(self: Symbol, alloc: std.mem.Allocator) void {
        alloc.free(self.inner_name);
        self.type.deinit(alloc);
    }
};

name: []const u8,

symbols: *std.StringHashMap(Symbol),
imports: *std.StringHashMap(Self),

pub fn init(alloc: std.mem.Allocator, name: []const u8) !Self {
    const symbols = try alloc.create(std.StringHashMap(Symbol));
    symbols.* = .init(alloc);

    const imports = try alloc.create(std.StringHashMap(Self));
    imports.* = .init(alloc);

    return .{
        .name = name,
        .symbols = symbols,
        .imports = imports,
    };
}

pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
    var symbols_it = self.symbols.valueIterator();
    while (symbols_it.next()) |sym| sym.deinit(alloc);
    self.symbols.deinit();
    alloc.destroy(self.symbols);

    var imports_it = self.imports.valueIterator();
    while (imports_it.next()) |sym| sym.deinit(alloc);
    self.imports.deinit();
    alloc.destroy(self.imports);
}
