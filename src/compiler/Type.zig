const std = @import("std");

const ast = @import("Parser").ast;

const Compiler = @import("Compiler.zig");

pub const Type = union(enum) {
    const Self = @This();

    pub const Function = struct {
        params: std.ArrayList(*const Self),
        return_type: *const Self,
    };

    fn CompoundType(T: enum { @"struct", @"enum", @"union" }) type {
        return struct {
            pub const MemberType = switch (T) {
                .@"struct", .@"union" => *const Self,
                .@"enum" => ?usize,
            };

            const Method = struct {
                inner_name: []const u8,
                params: std.ArrayList(*const Self),
                return_type: *const Self,
            };

            name: []const u8,
            members: *std.StringHashMap(MemberType),
            methods: *std.StringHashMap(Method),

            /// get member or method. returns `null` if no member or method is found with `name`.
            pub fn getProperty(self: *const CompoundType(T), name: []const u8) ?union(enum) {
                member: MemberType,
                method: Method,
            } {
                const member = self.members.get(name);
                const method = self.methods.get(name);

                return if (member) |m|
                    .{ .member = m }
                else if (method) |m|
                    .{ .method = m }
                else
                    null;
            }

            pub fn init(alloc: std.mem.Allocator, name: []const u8) !CompoundType(T) {
                const members = try alloc.create(std.StringHashMap(MemberType));
                members.* = .init(alloc);
                const methods = try alloc.create(std.StringHashMap(Method));
                methods.* = .init(alloc);
                return .{
                    .name = name,
                    .members = members,
                    .methods = methods,
                };
            }
        };
    }

    pub const Struct = CompoundType(.@"struct");
    pub const Union = CompoundType(.@"union");
    pub const Enum = CompoundType(.@"enum");

    pub const Reference = struct {
        inner: *const Self,
        is_mut: bool,
    };

    pub const Array = struct {
        inner: *const Self,
        /// if size is `null` type is an arraylist, else it's an array.
        /// if size is `_`, type is an array of inferred size.
        /// if size is a valid expression, type is an array of specified size.
        size: ?usize = null,
    };

    pub const ErrorUnion = struct {
        success: *const Self,
        @"error": ?*const Self = null,
    };

    i8,
    i16,
    i32,
    i64,

    u8,
    u16,
    u32,
    u64,

    f32,
    f64,

    bool,

    void,

    @"struct": Struct,
    @"enum": Enum,
    @"union": Union,
    optional: *const Self,
    reference: Reference,
    array: Array,
    error_union: ErrorUnion,
    function: Function,

    pub fn fromSymbol(symbol: []const u8) !Self {
        return if (std.mem.eql(u8, symbol, "i8"))
            .i8
        else if (std.mem.eql(u8, symbol, "i16"))
            .i16
        else if (std.mem.eql(u8, symbol, "i32"))
            .i32
        else if (std.mem.eql(u8, symbol, "i64"))
            .i64
        else if (std.mem.eql(u8, symbol, "u8"))
            .u8
        else if (std.mem.eql(u8, symbol, "u16"))
            .u16
        else if (std.mem.eql(u8, symbol, "u32"))
            .u32
        else if (std.mem.eql(u8, symbol, "u64"))
            .u64
        else if (std.mem.eql(u8, symbol, "f32"))
            .f32
        else if (std.mem.eql(u8, symbol, "f64"))
            .f64
        else if (std.mem.eql(u8, symbol, "void"))
            .void
        else if (std.mem.eql(u8, symbol, "bool"))
            .bool
        else
            error.TypeNotPrimitive;
    }

    /// Converts an AST type to a Compiler type.
    /// `infer_expr` is the expression with which the type is inferred.
    pub fn fromAst(compiler: *Compiler, t: union(enum) {
        strong: ast.Type,
        infer: ast.Expression,
    }) Compiler.CompilerError!Self {
        return switch (t) {
            .strong => |strong| switch (strong) {
                .symbol => |symbol| Self.fromSymbol(symbol) catch try compiler.getSymbolType(symbol),
                .reference => |reference| .{
                    .reference = .{
                        .inner = b: {
                            const ref_type = try compiler.alloc.create(Self);
                            ref_type.* = try fromAst(compiler, .{ .strong = reference.inner.* });
                            break :b ref_type;
                        },
                        .is_mut = reference.is_mut,
                    },
                },
                .function => |function| .{
                    .function = .{
                        .params = b: {
                            var params: std.ArrayList(*const Self) = try .initCapacity(
                                compiler.alloc,
                                function.parameters.items.len,
                            );
                            for (function.parameters.items) |p| {
                                const param = try compiler.alloc.create(Self);
                                param.* = try fromAst(compiler, .{ .strong = p.type });
                                params.appendAssumeCapacity(param);
                            }
                            break :b params;
                        },
                        .return_type = b: {
                            const return_type = try compiler.alloc.create(Self);
                            return_type.* = try fromAst(compiler, .{ .strong = function.return_type.* });
                            break :b return_type;
                        },
                    },
                },
                .array => |array| .{
                    .array = .{
                        .inner = b: {
                            const array_type = try compiler.alloc.create(Self);
                            array_type.* = try fromAst(compiler, .{ .strong = array.inner.* });
                            break :b array_type;
                        },
                        .size = (try compiler.solveComptimeExpression(if (array.size) |s|
                            s.*
                        else
                            @panic("can't infer array size"))).u64,
                    },
                },
                else => |other| std.debug.panic("unimplemented type {s}\n", .{@tagName(other)}),
            },
            .infer => |expr| try infer(compiler, expr),
        };
    }

    pub fn infer(compiler: *Compiler, expr: ast.Expression) !Self {
        return switch (expr) {
            .ident => |ident| try compiler.getSymbolType(ident),
            .int => |int| if (int <= std.math.maxInt(i32)) .i32 else .i64,
            .uint => |uint| if (uint <= std.math.maxInt(i32)) .i32 else .i64,
            .float => .f32,
            .char => .u8,
            .struct_instantiation => |struct_inst| try compiler.getSymbolType(struct_inst.name),
            .prefix => |prefix| try infer(compiler, prefix.rhs.*),
            .reference => |reference| .{
                .reference = .{
                    .inner = b: {
                        const inner = try compiler.alloc.create(Type);
                        inner.* = try .fromAst(compiler, .{ .infer = reference.inner.* });
                        break :b inner;
                    },
                    .is_mut = reference.is_mut,
                },
            },
            .array_instantiation => |array| .{
                .array = .{
                    .inner = b: {
                        const t = try compiler.alloc.create(Type);
                        t.* = try .fromAst(compiler, .{ .strong = array.type });
                        break :b t;
                    },
                    .size = if (array.length.* == .ident and std.mem.eql(u8, array.length.ident, "_"))
                        array.contents.items.len
                    else
                        (try compiler.solveComptimeExpression(array.length.*)).u64,
                },
            },
            else => |other| std.debug.panic("unimplemented type: {s}\n", .{@tagName(other)}),
        };
    }
};
