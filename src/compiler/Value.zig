const ast = @import("ast");

const compiler = @import("compiler.zig");

const Compiler = compiler.Compiler;
const Type = compiler.Type;

pub const Value = union(enum) {
    uint: usize,
    int: isize,
    float: f64,
    bool: bool,

    type: Type,

    pub fn eval(expr: *const ast.Expression, c: *const Compiler) !Value {
        _ = c;
        return switch (expr.*) {
            .int => |int| .{ .uint = int.payload },
            else => @panic("unimplemented"),
        };
    }

    pub fn getType(self: Value) Type {
        return switch (self) {
            .uint => .usize,
            .int => .isize,
            .float => .f64,
            .bool => .bool,
            .type => .type,
        };
    }
};
