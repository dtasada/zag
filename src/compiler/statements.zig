const std = @import("std");

const ast = @import("ast");

pub fn compile(alloc: std.mem.Allocator, statement: ast.Statement) ![]const u8 {
    // switch (statement) {}

    _ = alloc;
    _ = statement;
    unreachable;
}
