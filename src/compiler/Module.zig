const std = @import("std");

const utils = @import("utils");

const ast = @import("Parser").ast;

const Lexer = @import("Lexer");
const Parser = @import("Parser");
const Compiler = @import("Compiler.zig");
const Type = @import("Type.zig").Type;

const Self = @This();

const Symbol = struct {
    is_pub: bool,
    name: []const u8,
    type: Type,
};

name: []const u8,
symbols: std.StringHashMap(Symbol),

pub fn resolveSymbols(self: *Self, root_node: ast.RootNode) !void {
    for (root_node.items) |statement|
        try self.resolveStatement(statement);
}

fn resolveStatement(self: *Self, statement: ast.Statement) !void {
    switch (statement) {
        .binding_function_declaration => |func| try self.symbols.put(func.name, .{
            .name = func.name,
            .is_pub = func.is_pub,
            .type = undefined,
        }),
        else => {},
    }
}
