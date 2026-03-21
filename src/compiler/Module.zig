const std = @import("std");
const utils = @import("utils");

const Type = @import("type.zig").Type;
const Symbol = @import("compiler.zig").Symbol;

const Module = @This();

const Scope = std.ArrayList(Symbol);

scopes: std.ArrayList(*Scope) = .empty,

pub fn init(alloc: std.mem.Allocator) !Module {
    var self: Module = .{};

    try self.pushScope(alloc);

    try self.register(alloc, .{ .name = "i8", .type = .type, .binding = .@"const" });
    try self.register(alloc, .{ .name = "i16", .type = .type, .binding = .@"const" });
    try self.register(alloc, .{ .name = "i32", .type = .type, .binding = .@"const" });
    try self.register(alloc, .{ .name = "i64", .type = .type, .binding = .@"const" });
    try self.register(alloc, .{ .name = "isize", .type = .type, .binding = .@"const" });

    try self.register(alloc, .{ .name = "u8", .type = .type, .binding = .@"const" });
    try self.register(alloc, .{ .name = "u16", .type = .type, .binding = .@"const" });
    try self.register(alloc, .{ .name = "u32", .type = .type, .binding = .@"const" });
    try self.register(alloc, .{ .name = "u64", .type = .type, .binding = .@"const" });
    try self.register(alloc, .{ .name = "usize", .type = .type, .binding = .@"const" });

    try self.register(alloc, .{ .name = "void", .type = .type, .binding = .@"const" });
    try self.register(alloc, .{ .name = "bool", .type = .type, .binding = .@"const" });
    try self.register(alloc, .{ .name = "type", .type = .type, .binding = .@"const" });

    try self.register(alloc, .{ .name = "c_char", .type = .type, .binding = .@"const" });
    try self.register(alloc, .{ .name = "c_short", .type = .type, .binding = .@"const" });
    try self.register(alloc, .{ .name = "c_int", .type = .type, .binding = .@"const" });
    try self.register(alloc, .{ .name = "c_long", .type = .type, .binding = .@"const" });

    try self.register(alloc, .{ .name = "c_uchar", .type = .type, .binding = .@"const" });
    try self.register(alloc, .{ .name = "c_ushort", .type = .type, .binding = .@"const" });
    try self.register(alloc, .{ .name = "c_uint", .type = .type, .binding = .@"const" });
    try self.register(alloc, .{ .name = "c_ulong", .type = .type, .binding = .@"const" });

    return self;
}

pub fn deinit(self: *Module, alloc: std.mem.Allocator) void {
    for (0..self.scopes.items.len) |_| self.popScope(alloc);
    self.scopes.deinit(alloc);
}

pub fn register(self: *Module, alloc: std.mem.Allocator, symbol: Symbol) !void {
    try self.scopes.getLast().append(alloc, symbol);
}

pub fn pushScope(self: *Module, alloc: std.mem.Allocator) !void {
    const new_scope = try alloc.create(Scope);
    new_scope.* = .empty;
    try self.scopes.append(alloc, new_scope);
}

pub fn popScope(self: *Module, alloc: std.mem.Allocator) void {
    const last_scope = self.scopes.pop().?;
    for (last_scope.items) |symbol| symbol.deinit(alloc);
    last_scope.deinit(alloc);
    alloc.destroy(last_scope);
}

pub fn getSymbol(self: *const Module, name: []const u8) ?Symbol {
    for (1..self.scopes.items.len + 1) |i| {
        const scope = self.scopes.items[self.scopes.items.len - i];
        for (scope.items) |symbol| if (std.mem.eql(u8, symbol.name, name)) return symbol;
    }

    return null;
}
