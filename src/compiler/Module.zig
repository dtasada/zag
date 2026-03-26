const std = @import("std");
const utils = @import("utils");
const ast = @import("ast");

const compiler = @import("compiler.zig");
const errors = @import("errors.zig");
const Type = @import("type.zig").Type;
const Value = @import("value.zig").Value;
const Symbol = compiler.Symbol;
const Compiler = compiler.Compiler;

const Module = @This();

const Scope = std.ArrayList(Symbol);

name: []const u8,
scopes: std.ArrayList(*Scope) = .empty,

fn registerBuiltin(
    self: *Module,
    alloc: std.mem.Allocator,
    comptime name: []const u8,
    comptime inner_name: []const u8,
    value: Value,
) !void {
    try self.register(alloc, .{
        .name = name,
        .inner_name = inner_name,
        .type = .type,
        .binding = .@"const",
        .value = value,
        .free_inner_name = false,
        .free_type = true,
    });
}

pub fn init(alloc: std.mem.Allocator, name: []const u8) !Module {
    var self: Module = .{ .name = name };

    try self.pushScope(alloc);

    try self.registerBuiltin(alloc, "i8", "int8_t", .{ .type = .i8 });
    try self.registerBuiltin(alloc, "i16", "int16_t", .{ .type = .i16 });
    try self.registerBuiltin(alloc, "i32", "int32_t", .{ .type = .i32 });
    try self.registerBuiltin(alloc, "i64", "int64_t", .{ .type = .i64 });
    try self.registerBuiltin(alloc, "isize", "ptrdiff_t", .{ .type = .isize });

    try self.registerBuiltin(alloc, "u8", "uint8_t", .{ .type = .u8 });
    try self.registerBuiltin(alloc, "u16", "uint16_t", .{ .type = .u16 });
    try self.registerBuiltin(alloc, "u32", "uint32_t", .{ .type = .u32 });
    try self.registerBuiltin(alloc, "u64", "uint64_t", .{ .type = .u64 });
    try self.registerBuiltin(alloc, "usize", "size_t", .{ .type = .usize });

    try self.registerBuiltin(alloc, "f32", "float", .{ .type = .f32 });
    try self.registerBuiltin(alloc, "f64", "double", .{ .type = .f64 });

    try self.registerBuiltin(alloc, "void", "void", .{ .type = .void });
    try self.registerBuiltin(alloc, "bool", "bool", .{ .type = .bool });
    try self.registerBuiltin(alloc, "type", "type_type", .{ .type = .type });

    try self.registerBuiltin(alloc, "c_char", "char", .{ .type = .c_char });
    try self.registerBuiltin(alloc, "c_short", "short", .{ .type = .c_short });
    try self.registerBuiltin(alloc, "c_int", "int", .{ .type = .c_int });
    try self.registerBuiltin(alloc, "c_long", "long", .{ .type = .c_long });

    try self.registerBuiltin(alloc, "c_uchar", "unsigned char", .{ .type = .c_uchar });
    try self.registerBuiltin(alloc, "c_ushort", "unsigned short", .{ .type = .c_ushort });
    try self.registerBuiltin(alloc, "c_uint", "unsigned int", .{ .type = .c_uint });
    try self.registerBuiltin(alloc, "c_ulong", "unsigned long", .{ .type = .c_ulong });

    try self.registerBuiltin(alloc, "c_float", "float", .{ .type = .c_float });
    try self.registerBuiltin(alloc, "c_double", "double", .{ .type = .c_double });

    try self.registerBuiltin(alloc, "c_null", "NULL", .{ .type = .{ .reference = .{ .is_mut = false, .inner = &.void } } });
    try self.registerBuiltin(alloc, "nil", "nil", .{ .type = .@"typeof(nil)" });
    try self.registerBuiltin(alloc, "undefined", "undefined", .{ .type = .@"typeof(undefined)" });

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

pub fn getExpressionMutability(
    self: *const Module,
    alloc: std.mem.Allocator,
    expr: *const ast.Expression,
    c: *Compiler,
) errors.Error!bool {
    return switch (expr.*) {
        .ident => |ident| {
            const symbol = self.getSymbol(ident.payload) orelse return error.UnknownSymbol;
            return symbol.binding == .let_mut or
                (symbol.type == .slice and symbol.type.slice.is_mut);
        },
        .index => |index| {
            const t: Type = try .infer(alloc, index.lhs, c);
            defer t.deinit(alloc);
            if (t != .reference and t != .array)
                return errors.illegalIndex(t, c.source_map[index.pos]);

            const i_t: Type = try .infer(alloc, index.lhs, c);
            defer i_t.deinit(alloc);
            if (!i_t.isInteger())
                return errors.illegalIndexType(i_t, c.source_map[index.pos]);

            return try self.getExpressionMutability(alloc, index.lhs, c);
        },
        .dereference => |deref| {
            const t: Type = try .infer(alloc, deref.parent, c);
            defer t.deinit(alloc);
            if (t != .reference) return errors.derefNonPtr(t, c.source_map[deref.pos]);

            return t.reference.is_mut;
        },
        .member => |member| {
            const parent_t: Type = try .infer(alloc, member.parent, c);
            defer parent_t.deinit(alloc);

            return switch (parent_t) {
                inline .@"struct", .@"union" => try self.getExpressionMutability(alloc, member.parent, c),
                .@"enum" => false,
                .slice => if (std.mem.eql(u8, member.member_name, "ptr") or
                    std.mem.eql(u8, member.member_name, "len"))
                    true
                else
                    errors.badMemberAccessSlice(parent_t, member.member_name, c.source_map[member.pos]),
                else => errors.badMemberAccess(parent_t, member.member_name, c.source_map[member.pos]),
            };
        },
        else => false,
    };
}

pub fn getSymbolFromExpression(
    self: *const Module,
    expr: *const ast.Expression,
) ?union(enum) {
    success: Symbol,
    failure: []const u8,
} {
    return switch (expr.*) {
        .ident => |ident| if (self.getSymbol(ident.payload)) |symbol|
            .{ .success = symbol }
        else
            .{ .failure = ident.payload },
        .type => |t| switch (t.payload) {
            .symbol => |symbol| if (self.getSymbol(symbol.inner)) |s|
                .{ .success = s }
            else
                .{ .failure = symbol.inner },
            else => null,
        },
        else => null,
    };
}
