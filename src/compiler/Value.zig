const ast = @import("ast");

const compiler = @import("compiler.zig");

const Compiler = compiler.Compiler;

pub const Value = union(enum) {
    uint: usize,
    int: isize,
    float: f64,

    pub fn eval(c: Compiler, expr: *const ast.Expression) !Value {
        _ = c;
        return switch (expr.*) {
            .int => |int| .{ .uint = int.payload },
            else => @panic("unimplemented"),
        };
    }
};
