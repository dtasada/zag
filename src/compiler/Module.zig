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

const Scope = struct {
    symbols: std.ArrayList(*Symbol),
    defers: std.ArrayList(ast.Statement),
};

name: []const u8,
source_map: []const utils.Position,
scopes: std.ArrayList(*Scope) = .empty,
instantiations: std.StringHashMap(Type),

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
        .free_name = false,
        .free_inner_name = false,
        .free_type = true,
    });
}

/// Takes ownership of `source_map`
pub fn init(alloc: std.mem.Allocator, name: []const u8, source_map: []const utils.Position) !Module {
    var self: Module = .{
        .name = try alloc.dupe(u8, name),
        .source_map = source_map,
        .instantiations = .init(alloc),
    };
    errdefer self.deinit(alloc);

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

    try self.registerBuiltin(alloc, "c_null", "NULL", .{
        .type = .{
            .reference = .{
                .is_mut = false,
                .inner = try .clonePtr(.void, alloc),
            },
        },
    });
    try self.registerBuiltin(alloc, "nil", "nil", .nil);
    try self.registerBuiltin(alloc, "undefined", "undefined", .undefined);

    try self.register(alloc, .{
        .name = "cast",
        .inner_name = "cast",
        .type = .{ .template = .builtin_cast },
        .binding = .@"const",
        .free_name = false,
        .free_inner_name = false,
        .free_type = false,
    });

    try self.register(alloc, .{
        .name = "sizeof",
        .inner_name = "sizeof",
        .type = .{ .template = .builtin_sizeof },
        .binding = .@"const",
        .free_name = false,
        .free_inner_name = false,
        .free_type = false,
    });

    return self;
}

pub fn deinit(self: *Module, alloc: std.mem.Allocator) void {
    for (self.scopes.items) |_| self.popScope(alloc);
    self.scopes.deinit(alloc);

    var it = self.instantiations.iterator();
    while (it.next()) |entry| {
        alloc.free(entry.key_ptr.*);
        entry.value_ptr.deinit(alloc);
    }
    self.instantiations.deinit();
    alloc.free(self.name);
    alloc.free(self.source_map);
}

pub fn registerAtTopLevel(self: *Module, alloc: std.mem.Allocator, symbol: Symbol) !void {
    var new_symbol = symbol;
    if (!new_symbol.free_name) {
        const old_name = new_symbol.name;
        new_symbol.name = try alloc.dupe(u8, old_name);
        new_symbol.free_name = true;
    }
    if (!new_symbol.free_inner_name) {
        const old_inner = new_symbol.inner_name;
        new_symbol.inner_name = try alloc.dupe(u8, old_inner);
        new_symbol.free_inner_name = true;
    }
    const symbol_ptr = try alloc.create(Symbol);
    errdefer alloc.destroy(symbol_ptr);
    symbol_ptr.* = new_symbol;
    try self.scopes.items[0].symbols.append(alloc, symbol_ptr);
}

pub fn register(self: *Module, alloc: std.mem.Allocator, symbol: Symbol) !void {
    // If the symbol already owns its metadata, we take it.
    // If not, we dupe the name.
    var new_symbol = symbol;
    if (!new_symbol.free_name) {
        const old_name = new_symbol.name;
        new_symbol.name = try alloc.dupe(u8, old_name);
        new_symbol.free_name = true;
    }
    // We should also ensure inner_name is owned if it's not already.
    if (!new_symbol.free_inner_name) {
        const old_inner = new_symbol.inner_name;
        new_symbol.inner_name = try alloc.dupe(u8, old_inner);
        new_symbol.free_inner_name = true;
    }
    const symbol_ptr = try alloc.create(Symbol);
    errdefer alloc.destroy(symbol_ptr);
    symbol_ptr.* = new_symbol;
    try self.scopes.getLast().symbols.append(alloc, symbol_ptr);
}

pub fn registerPtrAtTopLevel(self: *Module, alloc: std.mem.Allocator, symbol: *Symbol) !void {
    if (!symbol.free_name) {
        const old_name = symbol.name;
        symbol.name = try alloc.dupe(u8, old_name);
        symbol.free_name = true;
    }
    if (!symbol.free_inner_name) {
        const old_inner = symbol.inner_name;
        symbol.inner_name = try alloc.dupe(u8, old_inner);
        symbol.free_inner_name = true;
    }
    try self.scopes.items[0].symbols.append(alloc, symbol);
}

pub fn registerPtr(self: *Module, alloc: std.mem.Allocator, symbol: *Symbol) !void {
    // We assume the caller gives us a pointer they WANT us to own and manage.
    if (!symbol.free_name) {
        const old_name = symbol.name;
        symbol.name = try alloc.dupe(u8, old_name);
        symbol.free_name = true;
    }
    if (!symbol.free_inner_name) {
        const old_inner = symbol.inner_name;
        symbol.inner_name = try alloc.dupe(u8, old_inner);
        symbol.free_inner_name = true;
    }
    try self.scopes.getLast().symbols.append(alloc, symbol);
}

pub fn pushScope(self: *Module, alloc: std.mem.Allocator) !void {
    const new_scope = try alloc.create(Scope);
    errdefer alloc.destroy(new_scope);
    new_scope.defers = .empty;
    new_scope.symbols = .empty;
    try self.scopes.append(alloc, new_scope);
}

pub fn popScope(self: *Module, alloc: std.mem.Allocator) void {
    const last_scope = self.scopes.pop().?;
    for (last_scope.symbols.items) |symbol| {
        symbol.deinit(alloc);
        alloc.destroy(symbol);
    }
    last_scope.symbols.deinit(alloc);
    last_scope.defers.deinit(alloc);
    alloc.destroy(last_scope);
}

pub fn getSymbol(self: *const Module, name: []const u8) ?*Symbol {
    var it = std.mem.reverseIterator(self.scopes.items);
    while (it.next()) |scope| {
        for (scope.symbols.items) |symbol| {
            if (std.mem.eql(u8, symbol.name, name)) {
                return symbol;
            }
        }
    }

    return null;
}

pub fn getExpressionMutability(
    self: *const Module,
    alloc: std.mem.Allocator,
    io: std.Io,
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
            const t: Type = try .infer(alloc, io, index.lhs, c);
            defer t.deinit(alloc);
            if (t != .reference and t != .array and t != .slice)
                return errors.illegalIndex(io, t, c.getPos(index.pos));

            const i_t: Type = try .infer(alloc, io, index.index, c);
            defer i_t.deinit(alloc);
            if (!i_t.isInteger())
                return errors.illegalIndexType(io, i_t, c.getPos(index.pos));

            return try self.getExpressionMutability(alloc, io, index.lhs, c);
        },
        .dereference => |deref| {
            const t: Type = try .infer(alloc, io, deref.parent, c);
            defer t.deinit(alloc);
            if (t != .reference) return errors.derefNonPtr(io, t, c.getPos(deref.pos));

            return t.reference.is_mut;
        },
        .member => |member| {
            const parent_t: Type = try .infer(alloc, io, member.parent, c);
            defer parent_t.deinit(alloc);

            return switch (parent_t) {
                inline .@"struct", .@"union" => try self.getExpressionMutability(alloc, io, member.parent, c),
                .@"enum" => false,
                .slice => if (std.mem.eql(u8, member.member_name, "ptr") or
                    std.mem.eql(u8, member.member_name, "len"))
                    true
                else
                    errors.badMemberAccessSlice(io, parent_t, member.member_name, c.getPos(member.pos)),
                else => errors.badMemberAccess(io, parent_t, member.member_name, c.getPos(member.pos)),
            };
        },
        else => false,
    };
}

pub fn getSymbolFromExpression(
    self: *const Module,
    alloc: std.mem.Allocator,
    io: std.Io,
    expr: *const ast.Expression,
    c: *Compiler,
) ?*Symbol {
    return switch (expr.*) {
        .ident => |ident| self.getSymbol(ident.payload),
        .type => |t| switch (t.payload) {
            .symbol => |symbol| self.getSymbol(symbol.inner),
            else => null,
        },
        .generic => |generic| {
            const lhs_t = Type.infer(alloc, io, generic.lhs, c) catch return null;
            defer lhs_t.deinit(alloc);

            const template_name = switch (lhs_t) {
                .template => |t| switch (t) {
                    .builtin_cast => "cast",
                    .builtin_sizeof => "sizeof",
                    inline else => |d| d.name,
                },
                else => return null,
            };

            const mangled_name = Type.getMangledName(alloc, io, template_name, generic.arguments, c) catch return null;
            defer alloc.free(mangled_name);

            return self.getSymbol(mangled_name);
        },
        else => null,
    };
}

pub fn findSymbolByType(self: *const Module, t: Type) ?*Symbol {
    var it = std.mem.reverseIterator(self.scopes.items);
    while (it.next()) |scope| {
        for (scope.symbols.items) |symbol| {
            if (symbol.type == .type and
                symbol.value != null and
                symbol.value.? == .type and
                symbol.value.?.type.eql(t))
                return symbol
            else
                continue;
        }
    }

    return null;
}
