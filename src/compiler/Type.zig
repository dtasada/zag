const std = @import("std");

const utils = @import("utils");
const ast = @import("Parser").ast;

const expressions = @import("expressions.zig");
const errors = @import("errors.zig");

const Compiler = @import("Compiler.zig");
const Module = @import("Module.zig");
const CompilerError = Compiler.CompilerError;

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
                .@"enum" => usize,
            };

            const Method = struct {
                inner_name: []const u8,
                params: std.ArrayList(*const Self),
                return_type: *const Self,
            };

            name: []const u8,
            members: *std.StringArrayHashMap(MemberType),
            methods: *std.StringArrayHashMap(Method),

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
                const members = try alloc.create(std.StringArrayHashMap(MemberType));
                members.* = .init(alloc);
                const methods = try alloc.create(std.StringArrayHashMap(Method));
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
        size: usize,
    };

    pub const ErrorUnion = struct {
        success: *const Self,
        failure: *const Self,
    };

    i8,
    i16,
    i32,
    i64,

    u8,
    u16,
    u32,
    u64,

    c_char,
    c_int,

    usize,

    f32,
    f64,

    bool,

    void,

    type,

    @"typeof(null)",
    @"typeof(undefined)",

    @"struct": Struct,
    @"enum": Enum,
    @"union": Union,
    optional: *const Self,
    arraylist: *const Self,
    reference: Reference,
    array: Array,
    error_union: ErrorUnion,
    function: Function,
    module: Module,
    variadic,
    generic,

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
        else if (std.mem.eql(u8, symbol, "usize"))
            .usize
        else if (std.mem.eql(u8, symbol, "f32"))
            .f32
        else if (std.mem.eql(u8, symbol, "f64"))
            .f64
        else if (std.mem.eql(u8, symbol, "void"))
            .void
        else if (std.mem.eql(u8, symbol, "bool"))
            .bool
        else if (std.mem.eql(u8, symbol, "c_int"))
            .c_int
        else if (std.mem.eql(u8, symbol, "c_char"))
            .c_char
        else
            error.TypeNotPrimitive;
    }

    /// Converts an AST type to a Compiler type.
    /// `infer` is the expression with which the type is inferred.
    pub fn fromAst(compiler: *Compiler, t: ast.Type) Compiler.CompilerError!Self {
        return switch (t) {
            .symbol => |symbol| compiler.getSymbolType(symbol.symbol) catch return errors.unknownSymbol(
                symbol.symbol,
                symbol.position,
            ),
            .variadic => .variadic,
            .reference => |reference| .{
                .reference = .{
                    .inner = b: {
                        const ref_type = try compiler.alloc.create(Self);
                        ref_type.* = try fromAst(compiler, reference.inner.*);
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
                            param.* = try fromAst(compiler, p.type);
                            params.appendAssumeCapacity(param);
                        }
                        break :b params;
                    },
                    .return_type = b: {
                        const return_type = try compiler.alloc.create(Self);
                        return_type.* = try fromAst(compiler, function.return_type.*);
                        break :b return_type;
                    },
                },
            },
            .array => |array| .{
                .array = .{
                    .inner = b: {
                        const array_type = try compiler.alloc.create(Self);
                        array_type.* = try fromAst(compiler, array.inner.*);
                        break :b array_type;
                    },
                    .size = (try compiler.solveComptimeExpression(array.size.*)).u64,
                },
            },
            .arraylist => |arraylist| .{
                .arraylist = b: {
                    const t_ptr = try compiler.alloc.create(Type);
                    t_ptr.* = try fromAst(compiler, arraylist.inner.*);
                    break :b t_ptr;
                },
            },
            .optional => |optional| .{
                .optional = b: {
                    const t_ptr = try compiler.alloc.create(Type);
                    t_ptr.* = try fromAst(compiler, optional.inner.*);
                    break :b t_ptr;
                },
            },
            .error_union => |error_union| .{
                .error_union = b: {
                    const success = try compiler.alloc.create(Type);
                    success.* = try fromAst(compiler, error_union.success.*);

                    const failure: *Type = try compiler.alloc.create(Type);
                    failure.* = if (error_union.failure) |err|
                        try fromAst(compiler, err.*)
                    else
                        .{ .reference = .{ .inner = &.void, .is_mut = false } };

                    break :b .{ .success = success, .failure = failure };
                },
            },
            .inferred => unreachable,
        };
    }

    pub fn infer(compiler: *Compiler, expr: ast.Expression) CompilerError!Self {
        return switch (expr) {
            .ident => |ident| compiler.getSymbolType(ident.ident) catch return errors.unknownSymbol(
                ident.ident,
                expr.getPosition(),
            ),
            .string => .{ .reference = .{ .inner = &.c_char, .is_mut = false } },
            .char => .u8,
            .uint => |uint| if (uint.uint <= std.math.maxInt(i32)) .i32 else .i64,
            .int => |int| if (int.int <= std.math.maxInt(i32) and int.int >= std.math.minInt(i32))
                .i32
            else
                .i64,
            .float => |float| if (float.float == @as(f64, @floatCast(@as(f32, @floatCast(float.float)))))
                // if the float fits in an f32, then default to f32. if the float is too big,
                // use f64
                .f32
            else
                .f64,

            .call => |call| try inferCallExpression(compiler, call),
            .member => |member| try inferMemberExpression(compiler, member),
            .binary => |binary| {
                const lhs: Type = try .infer(compiler, binary.lhs.*);
                const rhs: Type = try .infer(compiler, binary.rhs.*);
                if (!lhs.eql(rhs)) return utils.printErr(
                    error.TypeMismatch,
                    "comperr: Mismatched types in binary expression: {f} {s} {f} ({f})\n",
                    .{ lhs, @tagName(binary.op), rhs, binary.lhs.getPosition() },
                    .red,
                );

                return switch (binary.op) {
                    .@"==",
                    .@">",
                    .@"<",
                    .@">=",
                    .@"<=",
                    .@"!=",
                    .@"and",
                    .@"or",
                    => .bool,

                    else => lhs,
                };
            },
            .comparison => |comp| {
                var current_type = try Type.infer(compiler, comp.left.*);
                for (comp.comparisons.items) |cmp| {
                    const next_type = try Type.infer(compiler, cmp.right.*);
                    if (!current_type.eql(next_type)) return utils.printErr(
                        error.TypeMismatch,
                        "comperr: Mismatched types in comparison: {f} {s} {f}\n",
                        .{ current_type, @tagName(cmp.op), next_type },
                        .red,
                    );
                    current_type = next_type;
                }
                return .bool;
            },
            .prefix => |prefix| try infer(compiler, prefix.rhs.*),
            .range => @panic("invalid"),
            .assignment => .void,
            .struct_instantiation => |struct_inst| Type.infer(compiler, struct_inst.type_expr.*),
            .array_instantiation => |array| try inferArrayInstantiationExpression(compiler, array),
            .block => .void,
            .@"if" => |@"if"| if (@"if".@"else") |@"else"| {
                const expected: Type = try .infer(compiler, @"if".body.*);
                const received: Type = try .infer(compiler, @"else".*);
                if (!expected.eql(received)) return utils.printErr(
                    error.TypeMismatch,
                    "comperr: Type mismatch in if expression at {f}: {f} and {f} are not compatible",
                    .{ @"if".pos, expected, received },
                    .red,
                );

                return expected;
            } else return utils.printErr(
                error.MissingElseClause,
                "comperr: If expression must contain an else clause ({f})\n",
                .{@"if".pos},
                .red,
            ),
            .index => |index| switch (try Type.infer(compiler, index.lhs.*)) {
                .array => |array| array.inner.*,
                else => |other| utils.printErr(
                    error.IllegalExpression,
                    "comperr: Illegal index expression on '{f}' at {f}.",
                    .{ other, index.lhs.getPosition() },
                    .red,
                ),
            },
            .reference => |reference| .{
                .reference = .{
                    .inner = b: {
                        const inner = try compiler.alloc.create(Type);
                        inner.* = try .infer(compiler, reference.inner.*);
                        break :b inner;
                    },
                    .is_mut = reference.is_mut,
                },
            },
            .generic => |generic| {
                _ = generic;
                @panic("unimplemented");
            },
            .bad_node => unreachable,
        };
    }

    fn inferCallExpression(compiler: *Compiler, call: ast.Expression.Call) !Self {
        return switch (call.callee.*) {
            .member => |m| switch (try inferMemberExpression(compiler, m)) {
                .function => |function| function.return_type.*,
                else => |other| utils.printErr(
                    error.IllegalExpression,
                    "comperr: Member expression on '{f}' is illegal ({f})\n",
                    .{ other, call.pos },
                    .red,
                ),
            },
            else => switch (try infer(compiler, call.callee.*)) {
                .function => |function| function.return_type.*,
                else => |t| utils.printErr(
                    error.IllegalExpression,
                    "comperr: Member expression on '{f}' is illegal ({f})\n",
                    .{ t, call.pos },
                    .red,
                ),
            },
        };
    }

    fn inferArrayInstantiationExpression(compiler: *Compiler, array: ast.Expression.ArrayInstantiation) !Self {
        const t = try compiler.alloc.create(Type);
        t.* = try .fromAst(compiler, array.type);

        const size = if (array.length.* == .ident and std.mem.eql(u8, array.length.ident.ident, "_"))
            array.contents.items.len
        else b: {
            const length = (try compiler.solveComptimeExpression(array.length.*)).u64;

            const expected_length = array.contents.items.len;
            if (expected_length < length) return utils.printErr(
                error.MissingArguments,
                "comperr: Too many items in array initializer list. Expected {}, received {} ({f})\n",
                .{ expected_length, length, array.pos },
                .red,
            ) else if (expected_length > length) return utils.printErr(
                error.TooManyArguments,
                "comperr: Missing items in array initializer list. Expected {}, received {} ({f})\n",
                .{ expected_length, length, array.pos },
                .red,
            );

            break :b length;
        };

        return .{
            .array = .{
                .inner = t,
                .size = size,
            },
        };
    }

    fn inferMemberExpression(compiler: *Compiler, member: ast.Expression.Member) !Self {
        const parent_type = try infer(compiler, member.parent.*);
        b: switch (parent_type) {
            .@"struct" => |@"struct"| {
                if (@"struct".getProperty(member.member_name)) |property| switch (property) {
                    .member => |m| return m.*,
                    .method => |method| return .{
                        .function = .{
                            .params = method.params,
                            .return_type = method.return_type,
                        },
                    },
                } else return utils.printErr(
                    error.UndeclaredProperty,
                    "comperr: '{f}' has no member '{s}' ({f})\n",
                    .{ parent_type, member.member_name, member.parent.getPosition() },
                    .red,
                );
            },
            .reference => |reference| continue :b reference.inner.*,
            .module => |module| if (module.symbols.get(member.member_name)) |symbol| {
                return symbol.type;
            } else return utils.printErr(
                error.UndeclaredProperty,
                "comperr: Module '{s}' has no member '{s}' ({f})\n",
                .{ module.name, member.member_name, member.pos },
                .red,
            ),
            .@"enum" => |@"enum"| if (@"enum".members.contains(member.member_name)) {
                return parent_type;
            } else return utils.printErr(
                error.UndeclaredProperty,
                "comperr: Enum '{s}' has no member '{s}' ({f})\n",
                .{ @"enum".name, member.member_name, member.pos },
                .red,
            ),
            else => |other| return utils.printErr(
                error.IllegalExpression,
                "comperr: Member expression on '{f}' is illegal ({f})\n",
                .{ other, member.pos },
                .red,
            ),
        }
    }

    pub fn eql(a: Type, b: Type) bool {
        var buf: [128]Context.Visited = undefined;
        var list: std.ArrayList(Context.Visited) = .initBuffer(&buf);
        var ctx: Context = .{ .visited = &list };
        return ctx.eql(a, b);
    }

    pub fn hash(self: Type) u64 {
        var buf: [128]Context.Visited = undefined;
        var list: std.ArrayList(Context.Visited) = .initBuffer(&buf);
        var ctx: Context = .{ .visited = &list };
        return ctx.hash(self);
    }

    /// Checks if a type is convertible to a destination type.
    /// That means that a type can automatically be cast to another.
    /// Examples are `i64` -> `i32` or `usize` -> `?usize`
    pub fn convertsTo(src: Type, dst: Type) bool {
        return switch (src) {
            .@"typeof(undefined)" => true,
            .@"typeof(null)" => dst == .optional,
            .reference => |src_ref| switch (dst) {
                .reference => |dst_ref| src_ref.inner.eql(dst_ref.inner.*) and
                    src_ref.is_mut or !dst_ref.is_mut,
                else => false,
            },
            else => switch (dst) {
                .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .usize => switch (src) {
                    .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .usize => true,
                    else => false,
                },
                .f32, .f64 => switch (src) {
                    .f32, .f64, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .usize => true,
                    else => false,
                },
                .optional => |inner| inner.convertsTo(src),
                else => src.eql(dst),
            },
        };
    }

    pub fn format(
        self: *const Self,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self.*) {
            inline .@"struct", .@"enum", .@"union" => |compound| _ = try writer.write(compound.name),
            .optional => |optional| try writer.print("?{f}", .{optional}),
            .reference => |reference| try writer.print("&{s}{f}", .{
                if (reference.is_mut) "mut " else "",
                reference.inner,
            }),
            .array => |array| {
                _ = try writer.write("[");
                try writer.print("{}", .{array.size});
                _ = try writer.write("]");
                try writer.print("{f}", .{array.inner});
            },
            .arraylist => |array| {
                _ = try writer.write("[");
                _ = try writer.write("]");
                try writer.print("{f}", .{array.*});
            },
            .error_union => |error_union| {
                try writer.print("{f}", .{error_union.failure});
                try writer.print("!{f}", .{error_union.success});
            },
            .function => |function| {
                _ = try writer.write("fn (");
                for (function.params.items, 1..) |param, i| {
                    try writer.print("{f}", .{param});
                    if (i < function.params.items.len) _ = try writer.write(", ");
                }
                try writer.print(") {f}", .{function.return_type});
            },
            else => _ = try writer.write(@tagName(self.*)),
        }
    }

    pub const Context = struct {
        pub const Visited = struct { *const anyopaque, *const anyopaque };
        visited: *std.ArrayList(Visited),

        pub fn hash(ctx: Context, t: Type) u64 {
            var h = std.hash.Wyhash.init(0);

            // hash the tag
            const tag = std.meta.activeTag(t);
            h.update(std.mem.asBytes(&tag));

            switch (t) {
                .optional => |inner| h.update(std.mem.asBytes(&ctx.hash(inner.*))),

                .reference => |r| {
                    h.update(std.mem.asBytes(&ctx.hash(r.inner.*)));
                    h.update(std.mem.asBytes(&r.is_mut));
                },

                .array => |a| {
                    h.update(std.mem.asBytes(&ctx.hash(a.inner.*)));
                    h.update(std.mem.asBytes(&a.size));
                },

                .error_union => |e| {
                    h.update(std.mem.asBytes(&ctx.hash(e.success.*)));
                    h.update(std.mem.asBytes(&ctx.hash(e.failure.*)));
                },

                .function => |f| {
                    for (f.params.items) |p| {
                        h.update(std.mem.asBytes(&ctx.hash(p.*)));
                    }
                    h.update(std.mem.asBytes(&ctx.hash(f.return_type.*)));
                },

                inline .@"struct", .@"union" => |ct| {
                    h.update(ct.name);

                    // members (order-independent)
                    var mit = ct.members.iterator();
                    while (mit.next()) |entry| {
                        var eh = std.hash.Wyhash.init(0);
                        eh.update(entry.key_ptr.*);
                        switch (t) {
                            inline .@"struct", .@"union" => eh.update(std.mem.asBytes(&ctx.hash(entry.value_ptr.*.*))),
                            else => unreachable,
                        }

                        const mixed = eh.final();
                        h.update(std.mem.asBytes(&mixed));
                    }

                    // methods (order-independent)
                    var it = ct.methods.iterator();
                    while (it.next()) |entry| {
                        var eh = std.hash.Wyhash.init(0);
                        eh.update(entry.key_ptr.*);

                        for (entry.value_ptr.params.items) |p| {
                            eh.update(std.mem.asBytes(&ctx.hash(p.*)));
                        }
                        eh.update(std.mem.asBytes(&ctx.hash(entry.value_ptr.return_type.*)));

                        const mixed = eh.final();
                        h.update(std.mem.asBytes(&mixed));
                    }
                },
                .@"enum" => |ct| {
                    h.update(ct.name);

                    // members (order-independent)
                    var mit = ct.members.iterator();
                    while (mit.next()) |entry| {
                        var eh = std.hash.Wyhash.init(0);
                        eh.update(entry.key_ptr.*);
                        eh.update(std.mem.asBytes(&entry.value_ptr.*));
                        const mixed = eh.final();
                        h.update(std.mem.asBytes(&mixed));
                    }

                    // methods (order-independent)
                    var it = ct.methods.iterator();
                    while (it.next()) |entry| {
                        var eh = std.hash.Wyhash.init(0);
                        eh.update(entry.key_ptr.*);

                        for (entry.value_ptr.params.items) |p| {
                            eh.update(std.mem.asBytes(&ctx.hash(p.*)));
                        }
                        eh.update(std.mem.asBytes(&ctx.hash(entry.value_ptr.return_type.*)));

                        const mixed = eh.final();
                        h.update(std.mem.asBytes(&mixed));
                    }
                },

                else => {}, // all primitives
            }

            return h.final();
        }

        pub fn eql(ctx: Context, a: Type, b: Type) bool {
            if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;

            return switch (a) {
                inline .@"struct", .@"union" => |ta| {
                    const b_members = switch (b) {
                        inline .@"struct", .@"union" => |t| t.members,
                        else => unreachable,
                    };
                    const b_methods = switch (b) {
                        inline .@"struct", .@"union" => |t| t.methods,
                        else => unreachable,
                    };
                    const b_name = switch (b) {
                        inline .@"struct", .@"union" => |t| t.name,
                        else => unreachable,
                    };

                    for (ctx.visited.items) |visit| {
                        if (visit.@"0" == @as(*const anyopaque, @ptrCast(ta.members)) and
                            visit.@"1" == @as(*const anyopaque, @ptrCast(b_members)) or
                            visit.@"1" == @as(*const anyopaque, @ptrCast(ta.members)) and
                                visit.@"0" == @as(*const anyopaque, @ptrCast(b_members)))
                            return true;
                    }

                    if (!std.mem.eql(u8, ta.name, b_name)) return false;

                    if (ta.members.count() != b_members.count()) return false;
                    if (ta.methods.count() != b_methods.count()) return false;

                    ctx.visited.appendAssumeCapacity(.{ ta.members, b_members });
                    defer _ = ctx.visited.pop();

                    var members = ta.members.iterator();
                    while (members.next()) |entry| {
                        const name = entry.key_ptr.*;
                        const a_member = entry.value_ptr.*;
                        switch (b) {
                            inline .@"struct", .@"union" => |t| if (!ctx.eql(
                                a_member.*,
                                (t.members.get(name) orelse return false).*,
                            )) return false,
                            else => unreachable,
                        }
                    }

                    var methods = ta.methods.iterator();
                    while (methods.next()) |entry| {
                        const name = entry.key_ptr.*;
                        const a_method = entry.value_ptr.*;
                        const b_method = b_methods.get(name) orelse return false;

                        for (0..a_method.params.items.len) |i|
                            if (!ctx.eql(a_method.params.items[i].*, b_method.params.items[i].*))
                                return false;

                        if (!ctx.eql(a_method.return_type.*, b_method.return_type.*))
                            return false;
                    }

                    return true;
                },
                .@"enum" => |ta| {
                    if (!std.mem.eql(u8, ta.name, b.@"enum".name)) return false;

                    if (ta.members.count() != b.@"enum".members.count()) return false;

                    var members = ta.members.iterator();
                    while (members.next()) |entry| {
                        const name = entry.key_ptr.*;
                        const a_member = entry.value_ptr.*;
                        if (a_member != (b.@"enum".members.get(name) orelse return false))
                            return false;
                    }

                    var methods = ta.methods.iterator();
                    while (methods.next()) |entry| {
                        const name = entry.key_ptr.*;
                        const a_method = entry.value_ptr.*;
                        const b_method = b.@"enum".methods.get(name) orelse return false;

                        for (0..a_method.params.items.len) |i| {
                            if (!ctx.eql(a_method.params.items[i].*, b_method.params.items[i].*)) return false;
                        }

                        if (!ctx.eql(a_method.return_type.*, b_method.return_type.*)) return false;
                    }

                    return true;
                },
                .optional => |ta| ctx.eql(ta.*, b.optional.*),
                .reference => |ta| ctx.eql(ta.inner.*, b.reference.inner.*) and ta.is_mut == b.reference.is_mut,
                .array => |ta| ctx.eql(ta.inner.*, b.array.inner.*) and ta.size == b.array.size,
                .error_union => |ta| ctx.eql(ta.success.*, b.error_union.success.*) and
                    ctx.eql(ta.failure.*, b.error_union.failure.*),
                .function => |ta| {
                    if (ta.params.items.len != b.function.params.items.len) return false;

                    for (0..ta.params.items.len) |i| {
                        if (!ctx.eql(ta.params.items[i].*, b.function.params.items[i].*)) return false;
                    }

                    return ctx.eql(ta.return_type.*, b.function.return_type.*);
                },

                else => true, // primitive types
            };
        }
    };
};
