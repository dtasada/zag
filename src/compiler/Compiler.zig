const std = @import("std");
const ast = @import("ast");
const utils = @import("utils");

const parser = @import("parser");
const lexer = @import("lexer");

pub const Module = @import("Module.zig");

pub fn emit(
    alloc: std.mem.Allocator,
    file_path: []const u8,
    registry: *std.StringHashMap(Module),
) !void {
    const tokens, const tokens_source_map = try lexer.tokenize(alloc, file_path);
    const root_node = try parser.parse(alloc, tokens, tokens_source_map);
    defer utils.deinitSlice(ast.Statement, root_node, alloc);

    _ = registry;
}
