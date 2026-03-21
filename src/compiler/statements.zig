const std = @import("std");
const Compiler = @import("compiler.zig").Compiler;

const ast = @import("ast");

pub fn compile(alloc: std.mem.Allocator, statement: ast.Statement) ![]const u8 {
    // switch (statement) {}

    _ = alloc;
    _ = statement;
    unreachable;
}

pub fn compileTopLevel(alloc: std.mem.Allocator, compiler: *Compiler, statement: ast.TopLevelStatement) !void {
    switch (statement) {
        .import => |s| try import(alloc, compiler, s),
        .binding_function_declaration => |bfd| {
            _ = bfd;
            // var out: std.ArrayList(u8) = .empty;
            // out.print(alloc, "", .{bfd.return_type});
        },
        else => {},
    }
}

fn import(alloc: std.mem.Allocator, compiler: *Compiler, statement: ast.TopLevelStatement.Import) !void {
    // const mod = void;
    // compiler.module.register(alloc, .{
    //     .name = import.alias orelse import.module_name[import.module_name.len - 1],
    //     .type = .{ .module = mod },
    //     .binding = .@"const",
    // });
    inline for (&.{ compiler.source.includes, compiler.header.includes }) |writer| {
        try writer.appendSlice(alloc, "#include <");
        for (statement.module_name, 0..) |submod, i| {
            try writer.appendSlice(alloc, submod);
            try writer.appendSlice(alloc, if (i == statement.module_name.len - 1) ".h>\n" else "/");
        }
    }
}
