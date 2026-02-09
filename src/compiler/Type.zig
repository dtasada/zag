const std = @import("std");

const utils = @import("utils");
const ast = @import("Parser").ast;

const statements = @import("statements.zig");
const expressions = @import("expressions.zig");
const errors = @import("errors.zig");

const Value = @import("Value.zig").Value;
const Compiler = @import("Compiler.zig");
const Module = @import("Module.zig");
const CompilerError = errors.CompilerError;

pub const Type = union(enum) {
    const Self = @This();

    pub const Function = struct {
        pub const Param = struct {
            name: []const u8,
            type: Type,
        };

        name: []const u8,
        inner_name: []const u8,
        params: std.ArrayList(Param),
        generic_params: std.ArrayList(Function.Param),
        return_type: *const Self,
        definition: ?*const ast.Statement.FunctionDefinition = null,
        module: Module,
        generic_instantiation: ?GenericInstantiation = null,
        is_bind: bool = false,

        pub fn fromMethod(comptime T: utils.CompoundTypeTag, method: CompoundType(T).Method, module: Module) Type {
            return .{
                .function = .{
                    .name = method.inner_name,
                    .inner_name = method.inner_name,
                    .params = method.params,
                    .generic_params = method.generic_params,
                    .return_type = method.return_type,
                    .definition = method.definition,
                    .module = module,
                },
            };
        }
    };

    pub const GenericInstantiation = struct {
        base_name: []const u8,
        args: []const Value,
    };

    pub const Subtype = struct {
        pub const Tag = union(enum) {
            @"struct": Struct,
            @"union": Union,
            @"enum": Enum,
        };

        is_pub: bool,
        type: Tag,
        inner_name: []const u8,
    };

    fn CompoundType(T: utils.CompoundTypeTag) type {
        return struct {
            pub const MemberType = switch (T) {
                .@"struct", .@"union" => *const Self,
                .@"enum" => usize,
            };

            pub const Definition = switch (T) {
                .@"struct" => ast.Statement.StructDeclaration,
                .@"union" => ast.Statement.UnionDeclaration,
                .@"enum" => ast.Statement.EnumDeclaration,
            };

            const Method = struct {
                name: []const u8,
                inner_name: []const u8,
                params: std.ArrayList(Function.Param),
                generic_params: std.ArrayList(Function.Param),
                return_type: *const Self,
                definition: ?*const ast.Statement.FunctionDefinition = null,
            };

            pub const Variable = struct {
                is_pub: bool,
                type: Type,
                inner_name: []const u8,
            };

            name: []const u8,
            inner_name: []const u8,
            variables: *std.StringHashMap(Variable),
            subtypes: *std.StringHashMap(Subtype),
            members: *std.StringArrayHashMap(MemberType),
            methods: *std.StringArrayHashMap(Method),
            generic_params: std.ArrayList(Function.Param),
            tag_type: ?*const Type, // only for unions and enums
            definition: ?*const Definition,
            module: Module,
            generic_instantiation: ?GenericInstantiation = null,

            /// get member or method. returns `null` if no member or method is found with `name`.
            pub fn getProperty(self: *const CompoundType(T), name: []const u8) ?union(enum) {
                variable: Variable,
                member: MemberType,
                method: Method,
            } {
                const member = self.members.get(name);
                const method = self.methods.get(name);
                const variable = self.variables.get(name);

                return if (member) |m|
                    .{ .member = m }
                else if (method) |m|
                    .{ .method = m }
                else if (variable) |v|
                    .{ .variable = v }
                else
                    null;
            }

            pub fn getMember(self: *const CompoundType(T), tag_name: []const u8) !struct {
                member_name: []const u8,
                member_type: MemberType,
            } {
                var members_it = self.members.iterator();
                var i: usize = 0;
                while (members_it.next()) |m| : (i += 1) {
                    if (std.mem.eql(u8, tag_name, m.key_ptr.*))
                        return .{
                            .member_name = m.key_ptr.*,
                            .member_type = m.value_ptr.*,
                        };
                }

                return error.NoSuchMember;
            }

            pub fn init(compiler: *Compiler, name: []const u8, inner_name: []const u8, tag_type: ?Type) !CompoundType(T) {
                if (T == .@"struct" and tag_type != null) @panic("Struct type can't have a tag type");

                const variables = try compiler.alloc.create(std.StringHashMap(Variable));
                variables.* = .init(compiler.alloc);

                const subtypes = try compiler.alloc.create(std.StringHashMap(Subtype));
                subtypes.* = .init(compiler.alloc);

                const members = try compiler.alloc.create(std.StringArrayHashMap(MemberType));
                members.* = .init(compiler.alloc);

                const methods = try compiler.alloc.create(std.StringArrayHashMap(Method));
                methods.* = .init(compiler.alloc);

                var tag: ?*Type = null;
                if (tag_type) |t| {
                    tag = try compiler.alloc.create(Type);
                    tag.?.* = t;
                }

                return .{
                    .name = name,
                    .inner_name = inner_name,
                    .variables = variables,
                    .subtypes = subtypes,
                    .members = members,
                    .methods = methods,
                    .generic_params = .empty,
                    .tag_type = tag,
                    .definition = null,
                    .module = compiler.module,
                    .generic_instantiation = null,
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

    pub const Slice = Reference;

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
    c_long,
    c_short,
    c_int,

    c_uchar,
    c_ulong,
    c_ushort,
    c_uint,

    usize,

    f32,
    f64,

    bool,

    void,

    type: ?*const Type,
    any,

    @"typeof(null)",
    @"typeof(undefined)",
    generic_param: []const u8,

    @"struct": Struct,
    @"enum": Enum,
    @"union": Union,
    optional: *const Self,
    slice: Slice,
    reference: Reference,
    array: Array,
    error_union: ErrorUnion,
    function: Function,
    module: Module,
    variadic,

    /// Converts an AST type to a Compiler type.
    /// `infer` is the expression with which the type is inferred.
    pub fn fromAst(compiler: *Compiler, t: ast.Type) CompilerError!Self {
        return switch (t) {
            .symbol => |symbol| compiler.getSymbolType(symbol.symbol) catch return errors.unknownSymbol(
                symbol.symbol,
                symbol.pos,
            ),
            .generic => |generic| try instantiateGeneric(
                compiler,
                try fromAst(compiler, generic.lhs.*),
                generic.arguments,
                generic.pos,
            ),
            .variadic => .variadic,
            .reference => |reference| .{
                .reference = .{
                    .inner = try .fromAstPtr(compiler, reference.inner.*),
                    .is_mut = reference.is_mut,
                },
            },
            .function => |function| .{
                .function = b: {
                    try compiler.pushScope();
                    defer compiler.popScope();

                    var generic_params: std.ArrayList(Function.Param) = try .initCapacity(compiler.alloc, function.generic_parameters.items.len);
                    for (function.generic_parameters.items) |p| {
                        generic_params.appendAssumeCapacity(.{
                            .name = p.name,
                            .type = if (p.type == .inferred)
                                .{ .type = null }
                            else
                                try .fromAst(compiler, p.type),
                        });
                        try compiler.registerSymbol(p.name, .{ .type = .{ .generic_param = p.name } }, .{});
                    }

                    var params: std.ArrayList(Function.Param) = try .initCapacity(compiler.alloc, function.parameters.items.len);
                    for (function.parameters.items) |p|
                        params.appendAssumeCapacity(.{
                            .name = p.name,
                            .type = try fromAst(compiler, p.type),
                        });

                    break :b .{
                        .name = function.name,
                        .inner_name = try compiler.mangle(function.name),
                        .params = params,
                        .generic_params = generic_params,
                        .return_type = try .fromAstPtr(compiler, function.return_type.*),
                        .module = compiler.module,
                    };
                },
            },
            .array => |array| .{
                .array = .{
                    .inner = try .fromAstPtr(compiler, array.inner.*),
                    .size = (try compiler.solveComptimeExpression(array.size.*)).u64,
                },
            },
            .slice => |slice| .{ .slice = .{ .is_mut = slice.is_mut, .inner = try .fromAstPtr(compiler, slice.inner.*) } },
            .optional => |optional| .{ .optional = try .fromAstPtr(compiler, optional.inner.*) },
            .error_union => |error_union| .{
                .error_union = b: {
                    const success: *Type = try .fromAstPtr(compiler, error_union.success.*);

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

    pub fn fromAstPtr(compiler: *Compiler, t: ast.Type) CompilerError!*Self {
        const ptr = try compiler.alloc.create(Self);
        ptr.* = try fromAst(compiler, t);
        return ptr;
    }

    fn instantiateGeneric(
        compiler: *Compiler,
        base_type: Type,
        arguments: ast.ArgumentList,
        pos: utils.Position,
    ) CompilerError!Self {
        var args: std.ArrayList(Value) = try .initCapacity(compiler.alloc, arguments.items.len);
        defer args.deinit(compiler.alloc);

        for (arguments.items) |arg|
            args.appendAssumeCapacity(try compiler.solveComptimeExpression(arg));

        // Check generic params
        const params = switch (base_type) {
            .@"struct" => |s| s.generic_params,
            .@"union" => |u| u.generic_params,
            .function => |f| f.generic_params,
            else => return utils.printErr(
                error.TypeNotGeneric,
                "comperr: Type {f} is not generic ({f})\n",
                .{ base_type, pos },
                .red,
            ),
        };

        if (params.items.len != args.items.len)
            return errors.genericArgumentCountMismatch(params.items.len, args.items.len, pos);

        // Mangle name
        const base_name = switch (base_type) {
            .@"struct" => |s| s.name,
            .@"union" => |u| u.name,
            .function => |f| f.name,
            else => unreachable,
        };

        // If the arguments are identical to the base type's own generic parameters,
        // this is a self-reference to the generic template (e.g. ArrayList<T> inside ArrayList<T>).
        // In this case, just return the base type.
        var all_match = true;
        for (params.items, 0..) |p, i| {
            const arg = args.items[i];
            if (arg != .type or
                arg.type != .generic_param or
                !std.mem.eql(u8, arg.type.generic_param, p.name))
            {
                all_match = false;
                break;
            }
        }
        if (all_match) return base_type;

        var mangled_name: std.ArrayList(u8) = .empty;
        defer mangled_name.deinit(compiler.alloc);

        _ = try mangled_name.writer(compiler.alloc).write(base_name);

        for (args.items) |arg| try mangled_name.writer(compiler.alloc).print("_{}", .{arg.hash()});

        const name = try compiler.alloc.dupe(u8, mangled_name.items);

        // Check if already instantiated
        if (compiler.getSymbolType(name)) |t| {
            return t;
        } else |_| {}

        // Instantiate
        const DefUnion = union(enum) {
            @"struct": *const ast.Statement.StructDeclaration,
            @"union": *const ast.Statement.UnionDeclaration,
            function: *const ast.Statement.FunctionDefinition,
        };
        const definition_wrapper: ?DefUnion = switch (base_type) {
            inline .@"struct", .@"union", .function => |s, t| if (s.definition) |d|
                @unionInit(DefUnion, @tagName(t), d)
            else
                null,
            else => null,
        };

        const module = switch (base_type) {
            .@"struct" => |s| s.module,
            .@"union" => |u| u.module,
            .function => |f| f.module,
            else => unreachable,
        };

        if (definition_wrapper) |def_wrap| {
            try compiler.pushScope();

            try compiler.registerSymbol(
                base_name,
                .{ .type = base_type },
                .{ .inner_name = try compiler.mangle(base_name) },
            );

            for (params.items, 0..) |param, i| {
                const val = args.items[i];
                switch (val) {
                    .type => |t| try compiler.registerSymbol(param.name, .{ .type = t }, .{}),
                    else => try compiler.registerSymbol(
                        param.name,
                        .{ .constant = .{ .type = val.getType(), .value = val } },
                        .{},
                    ),
                }
            }

            const new_type: Type = switch (def_wrap) {
                .@"struct" => |s| blk: {
                    var copy_decl = try s.clone(compiler.alloc);
                    copy_decl.name = name;
                    copy_decl.generic_types = .empty;

                    var t = try fromCompoundTypeDeclaration(compiler, .@"struct", &copy_decl);
                    t.module = module;
                    t.generic_instantiation = .{
                        .base_name = base_name,
                        .args = args.items,
                    };
                    break :blk .{ .@"struct" = t };
                },
                .@"union" => |d| blk: {
                    var copy = d.*;
                    copy.name = name;
                    copy.generic_types = .empty;
                    var t = try fromCompoundTypeDeclaration(compiler, .@"union", &copy);
                    t.module = module;
                    t.generic_instantiation = .{
                        .base_name = base_name,
                        .args = try compiler.alloc.dupe(Value, args.items),
                    };
                    break :blk .{ .@"union" = t };
                },
                .function => |d| blk: {
                    var copy = d.*;
                    copy.name = name;
                    copy.generic_parameters = .empty;
                    var t = try fromAst(compiler, copy.getType());
                    t.function.definition = d;
                    t.function.generic_instantiation = .{
                        .base_name = base_name,
                        .args = try compiler.alloc.dupe(Value, args.items),
                    };
                    break :blk t;
                },
            };

            compiler.popScope(); // Unregisters placeholders

            // Register globally
            var global_scope = &compiler.scopes.items[0];
            try global_scope.put(name, .{ .type = .{
                .type = new_type,
                .inner_name = name,
                .is_defined = false,
            } });

            // Add to pending instantiations
            try compiler.pending_instantiations.append(compiler.alloc, .{
                .inner_name = name,
                .args = try args.toOwnedSlice(compiler.alloc),
                .module = module,
                .t = switch (def_wrap) {
                    .@"struct" => |d| .{ .@"struct" = d.* },
                    .@"union" => |d| .{ .@"union" = d.* },
                    .function => |d| .{ .function = d.* },
                },
            });

            return new_type;
        } else if (base_type == .function and std.mem.eql(u8, base_type.function.name, "sizeof")) {
            var new_f = base_type.function;
            new_f.name = name;
            new_f.generic_params = .empty;
            new_f.generic_instantiation = .{
                .base_name = base_name,
                .args = try compiler.alloc.dupe(Value, args.items),
            };
            const new_type: Type = .{ .function = new_f };

            // Register globally
            var global_scope = &compiler.scopes.items[0];
            try global_scope.put(name, .{ .type = .{
                .type = new_type,
                .inner_name = name,
                .is_defined = true,
            } });

            return new_type;
        } else if (base_type == .function and std.mem.eql(u8, base_type.function.name, "cast")) {
            var new_f = base_type.function;
            new_f.name = name;
            new_f.generic_params = .empty;
            new_f.generic_instantiation = .{
                .base_name = base_name,
                .args = try compiler.alloc.dupe(Value, args.items),
            };

            // Set the return type to T (args[0])
            if (args.items.len > 0) {
                switch (args.items[0]) {
                    .type => |t| {
                        const ret_ptr = try compiler.alloc.create(Type);
                        ret_ptr.* = t;
                        new_f.return_type = ret_ptr;
                    },
                    else => {},
                }
            }

            const new_type: Type = .{ .function = new_f };

            // Register globally
            var global_scope = &compiler.scopes.items[0];
            try global_scope.put(name, .{ .type = .{
                .type = new_type,
                .inner_name = name,
                .is_defined = true,
            } });

            return new_type;
        } else if (base_type == .function and std.mem.eql(u8, base_type.function.name, "xor")) {
            var new_f = base_type.function;
            new_f.name = name;
            new_f.generic_params = .empty;
            new_f.generic_instantiation = .{
                .base_name = base_name,
                .args = try compiler.alloc.dupe(Value, args.items),
            };

            // Set the return type to T (args[0])
            if (args.items.len > 0) switch (args.items[0]) {
                .type => |t| {
                    const ret_ptr = try compiler.alloc.create(Type);
                    ret_ptr.* = t;
                    new_f.return_type = ret_ptr;
                },
                else => {},
            };

            const new_type: Type = .{ .function = new_f };

            // Register globally
            var global_scope = &compiler.scopes.items[0];
            try global_scope.put(name, .{ .type = .{
                .type = new_type,
                .inner_name = name,
                .is_defined = true,
            } });

            return new_type;
        } else return utils.printErr(
            error.GenericInstantiationFailed,
            "comperr: Cannot instantiate {f} (missing definition or unsupported) ({f})\n",
            .{ base_type, pos },
            .red,
        );
    }

    pub fn infer(compiler: *Compiler, expr: ast.Expression) CompilerError!Self {
        return switch (expr) {
            .ident => |ident| switch (compiler.getScopeItem(ident.ident) catch
                return errors.unknownSymbol(ident.ident, expr.getPosition())) {
                .symbol => |s| s.type,
                .type => |*t| .{ .type = &t.type },
                .module => |m| .{ .module = m },
                .constant => |c| c.type,
            },
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
            .@"try" => |t| {
                const inner_type = try infer(compiler, t.@"try".*);

                if (compiler.current_return_type.? != .error_union)
                    return errors.tryExpressionBadReturnType(inner_type, compiler.current_return_type.?, t.pos);

                return switch (inner_type) {
                    .error_union => |eu| eu.success.*,
                    else => |other| errors.tryExpressionOnNonErrorUnion(other, t.pos),
                };
            },
            .binary => |binary| {
                const lhs: Type = try .infer(compiler, binary.lhs.*);
                const rhs: Type = try .infer(compiler, binary.rhs.*);
                if (!rhs.check(lhs)) return utils.printErr(
                    error.TypeMismatch,
                    "comperr: Mismatched types in binary expression: '{f}' {s} '{f}' ({f}).\n",
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
                    if (!next_type.check(current_type)) return utils.printErr(
                        error.TypeMismatch,
                        "comperr: Mismatched types in comparison: '{f}' {s} '{f}' ({f}).\n",
                        .{ current_type, @tagName(cmp.op), next_type, comp.pos },
                        .red,
                    );
                    current_type = next_type;
                }
                return .bool;
            },
            .prefix => |prefix| try infer(compiler, prefix.rhs.*),
            .range => @panic("invalid"),
            .assignment => .void,
            .struct_instantiation => |struct_inst| try infer(compiler, struct_inst.type_expr.*),
            .array_instantiation => |array| try inferArrayInstantiationExpression(compiler, array),
            .block => |block| try inferBlock(compiler, block),
            .@"if" => |@"if"| if (@"if".@"else") |@"else"| {
                try compiler.pushScope();
                if (@"if".capture) |capture| {
                    const capture_type = switch (try infer(compiler, @"if".condition.*)) {
                        .optional => |optional| optional.*,
                        else => |other| other,
                    };

                    try compiler.registerSymbol(capture, .{ .symbol = .{ .type = capture_type } }, .{});
                }

                const expected: Type = try infer(compiler, @"if".body.*);
                if (@"if".capture) |_| compiler.popScope();

                const received: Type = try infer(compiler, @"else".*);
                if (!received.check(expected))
                    return errors.typeMismatchIfExpression(expected, received, @"if".pos);

                return expected;
            } else return errors.ifExpressionMustContainElseClause(@"if".pos),
            .index => |index| switch (try Type.infer(compiler, index.lhs.*)) {
                .array => |array| array.inner.*,
                .slice => |slice| slice.inner.*,
                else => |other| utils.printErr(
                    error.IllegalExpression,
                    "comperr: Illegal index expression on '{f}' ({f}).\n",
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
            .generic => |generic| b: {
                var base_type = try infer(compiler, generic.lhs.*);
                if (base_type == .type) {
                    base_type = (try compiler.solveComptimeExpression(generic.lhs.*)).type;
                }

                const t = try compiler.alloc.create(Self);
                t.* = try instantiateGeneric(
                    compiler,
                    base_type,
                    generic.arguments,
                    generic.pos,
                );

                break :b switch (t.*) {
                    .function => t.*,
                    else => .{ .type = t },
                };
            },
            .match => |_| @panic("unimplemented"),
            .type => |t| try fromAst(compiler, t),
            .slice => |slice| switch (try infer(compiler, slice.lhs.*)) {
                // TODO: change slice mutability to equal binding mutability of reference
                inline .slice, .array => |t| .{ .slice = .{ .inner = t.inner, .is_mut = false } },
                else => |other| errors.illegalSliceExpression(other, slice.pos),
            },
            .bad_node => unreachable,
        };
    }

    fn inferCallExpression(compiler: *Compiler, call: ast.Expression.Call) !Self {
        const callee_type: Type = try infer(compiler, call.callee.*);

        return switch (callee_type) {
            .function => |function| function.return_type.*,
            else => |t| errors.illegalCallExpression(t, call.pos),
        };
    }

    pub fn inferBlock(self: *Compiler, blk: ast.Expression.Block) !Type {
        try self.pushScope();
        defer self.popScope();

        for (blk.block.items) |statement| switch (statement) {
            .variable_definition => |vd| try self.registerSymbol(vd.variable_name, .{
                .symbol = .{ .type = try .infer(self, vd.assigned_value) },
            }, .{}),
            .block_eval => |expr| return try .infer(self, expr),
            else => {},
        };

        return .void;
    }

    /// Returns a type from an AST compound type declaration statement.
    pub fn fromCompoundTypeDeclaration(
        compiler: *Compiler,
        comptime T: utils.CompoundTypeTag,
        type_decl: *const switch (T) {
            .@"struct" => ast.Statement.StructDeclaration,
            .@"union" => ast.Statement.UnionDeclaration,
            .@"enum" => ast.Statement.EnumDeclaration,
        },
    ) CompilerError!switch (T) {
        .@"struct" => Type.Struct,
        .@"union" => Type.Union,
        .@"enum" => Type.Enum,
    } {
        const inner_name = try compiler.mangle(type_decl.name);
        const tag_type_inner_name: []const u8 = try std.fmt.allocPrint(compiler.alloc, "{s}_tag_type", .{type_decl.name});

        const should_define = if (compiler.getSymbolDefined(type_decl.name)) |sd| b: {
            if (sd)
                return errors.symbolShadowing(type_decl.name, type_decl.pos)
            else
                break :b false;
        } else |err| switch (err) {
            error.SymbolNotVariable => return err,
            else => true,
        };

        // register struct in scope
        var compound_type: switch (T) {
            .@"struct" => Type.Struct,
            .@"union" => Type.Union,
            .@"enum" => Type.Enum,
        } = if (compiler.getSymbolType(type_decl.name)) |existing| b: {
            switch (T) {
                .@"struct" => if (existing == .@"struct" and existing.@"struct".definition != null) break :b existing.@"struct",
                .@"union" => if (existing == .@"union" and existing.@"union".definition != null) break :b existing.@"union",
                .@"enum" => if (existing == .@"enum" and existing.@"enum".definition != null) break :b existing.@"enum",
            }
            // If type mismatch or not found (logic error in scan?), create new.
            // But strict forward decl implies we should find it.
            // However, we fallback to init for safety/standalone usage.
            break :b try .init(compiler, type_decl.name, inner_name, switch (T) {
                .@"struct" => null,
                .@"enum" => getTagType(type_decl.members.items.len),
                .@"union" => b2: {
                    const enum_decl = try compiler.alloc.create(ast.Statement.EnumDeclaration);
                    enum_decl.* = .{
                        .pos = type_decl.pos,
                        .is_pub = false,
                        .name = tag_type_inner_name,
                        .variables = .empty,
                        .subtypes = .empty,
                        .members = .empty,
                        .methods = .empty,
                    };

                    for (type_decl.members.items) |member|
                        try enum_decl.members.append(compiler.alloc, .{ .name = member.name });

                    // Create the tag type but DON'T compile/emit it yet
                    // It will be emitted when the union itself is compiled in statements.zig
                    break :b2 .{ .@"enum" = try Type.fromCompoundTypeDeclaration(compiler, .@"enum", enum_decl) };
                },
            });
        } else |_| try .init(compiler, type_decl.name, inner_name, switch (T) {
            .@"struct" => null,
            .@"enum" => getTagType(type_decl.members.items.len),
            .@"union" => b: {
                const enum_decl = try compiler.alloc.create(ast.Statement.EnumDeclaration);
                enum_decl.* = .{
                    .pos = type_decl.pos,
                    .is_pub = false,
                    .name = tag_type_inner_name,
                    .variables = .empty,
                    .subtypes = .empty,
                    .members = .empty,
                    .methods = .empty,
                };

                for (type_decl.members.items) |member|
                    try enum_decl.members.append(compiler.alloc, .{ .name = member.name });

                // Create the tag type but DON'T compile/emit it yet
                break :b .{ .@"enum" = try Type.fromCompoundTypeDeclaration(compiler, .@"enum", enum_decl) };
            },
        });

        // If definition is set, it means we already populated this type.
        if (compound_type.definition != null) return compound_type;

        switch (T) {
            .@"struct", .@"union" => for (type_decl.generic_types.items) |g| {
                try compound_type.generic_params.append(compiler.alloc, .{
                    .name = g.name,
                    .type = if (g.type == .inferred)
                        .{ .type = null }
                    else
                        try .fromAst(compiler, g.type),
                });
            },
            else => {},
        }

        compound_type.definition = type_decl;

        try compiler.registerSymbol(
            type_decl.name,
            .{ .type = @unionInit(Type, @tagName(T), compound_type) },
            .{ .is_defined = should_define, .inner_name = try compiler.mangle(type_decl.name) },
        );

        try compiler.pushScope();
        defer compiler.popScope();

        switch (T) {
            .@"struct", .@"union" => for (type_decl.generic_types.items) |g|
                try compiler.registerSymbol(
                    g.name,
                    .{ .type = .{ .generic_param = g.name } },
                    .{},
                ),
            else => {},
        }

        for (type_decl.variables.items) |variable|
            try compound_type.variables.put(
                variable.variable_name,
                .{
                    .is_pub = variable.is_pub,
                    .inner_name = try std.fmt.allocPrint(
                        compiler.alloc,
                        "{s}_{s}",
                        .{ inner_name, variable.variable_name },
                    ),
                    .type = if (variable.type == .inferred)
                        try infer(compiler, variable.assigned_value)
                    else
                        try fromAst(compiler, variable.type),
                },
            );

        for (type_decl.subtypes.items) |subtype| switch (subtype) {
            inline else => |st, tag| try compound_type.subtypes.put(
                st.name,
                .{
                    .is_pub = st.is_pub,
                    .inner_name = try std.fmt.allocPrint(compiler.alloc, "{s}_{s}", .{ inner_name, st.name }),
                    .type = @unionInit(Type.Subtype.Tag, @tagName(tag), try fromCompoundTypeDeclaration(compiler, tag, &st)),
                },
            ),
        };

        var enum_last_value: usize = 0;
        for (type_decl.members.items) |member| {
            if (compound_type.getProperty(member.name)) |_| return utils.printErr(
                error.DuplicateMember,
                "comperr: Duplicate member '{s}' declared in '{s}' at {f}.\n",
                .{ member.name, type_decl.name, type_decl.pos },
                .red,
            );

            switch (T) {
                inline .@"struct", .@"union" => {
                    const member_type: *const Type = switch (T) {
                        .@"struct" => try fromAstPtr(compiler, member.type),
                        .@"union" => if (member.type) |t| try fromAstPtr(compiler, t) else &.void,
                        else => unreachable,
                    };
                    switch (member_type.*) {
                        inline .@"struct", .@"union" => |ct, tag| {
                            var inner_members = ct.members.iterator();
                            while (inner_members.next()) |inner_member| {
                                if (inner_member.value_ptr.*.eql(@unionInit(Type, @tagName(T), compound_type)))
                                    return utils.printErr(
                                        error.CircularTypeDefinition,
                                        "comperr: {s} '{s}' depends on itself. Member '{s}' is of type '{s}' ({f}).\n",
                                        .{ @tagName(tag), compound_type.name, inner_member.key_ptr.*, compound_type.name, type_decl.pos },
                                        .red,
                                    );
                            }
                        },
                        else => {},
                    }

                    try compound_type.members.put(member.name, member_type);
                },
                .@"enum" => try compound_type.members.put(member.name, if (member.value) |value|
                    (try compiler.solveComptimeExpression(value)).u64
                else b: {
                    const val = enum_last_value;
                    enum_last_value += 1;
                    break :b val;
                }),
            }
        }

        for (type_decl.methods.items, 0..) |method, i| {
            var params: std.ArrayList(Function.Param) = .empty;
            try params.ensureTotalCapacity(compiler.alloc, method.parameters.items.len);

            for (method.parameters.items) |p|
                params.appendAssumeCapacity(.{
                    .name = p.name,
                    .type = try .fromAst(compiler, p.type),
                });

            var generic_params: std.ArrayList(Function.Param) = .empty;
            try generic_params.ensureTotalCapacity(compiler.alloc, method.generic_parameters.items.len);
            for (method.generic_parameters.items) |p|
                generic_params.appendAssumeCapacity(.{
                    .name = p.name,
                    .type = if (p.type == .inferred)
                        .{ .type = null }
                    else
                        try .fromAst(compiler, p.type),
                });

            const return_type = try fromAstPtr(compiler, method.return_type);
            try compound_type.methods.put(method.name, .{
                .name = method.name,
                .inner_name = try std.fmt.allocPrint(compiler.alloc, "__zag_{s}_{s}", .{
                    type_decl.name,
                    method.name,
                }),
                .generic_params = generic_params,
                .params = params,
                .return_type = return_type,
                .definition = &type_decl.methods.items[i],
            });
        }

        return compound_type;
    }

    fn inferArrayInstantiationExpression(compiler: *Compiler, array: ast.Expression.ArrayInstantiation) !Self {
        const t = try fromAstPtr(compiler, array.type);

        const size = if (array.length.* == .ident and std.mem.eql(u8, array.length.ident.ident, "_"))
            array.contents.items.len
        else b: {
            const expected_length = (try compiler.solveComptimeExpression(array.length.*)).u64;

            const received_length = array.contents.items.len;
            if (received_length != expected_length) return utils.printErr(
                error.ArgumentCountMismatch,
                "comperr: Expected {} items in array initializer list, found {} ({f}).\n",
                .{ expected_length, received_length, array.pos },
                .red,
            );

            break :b expected_length;
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
                    .variable => |v| return v.type,
                    .member => |m| return m.*,
                    .method => |method| return Type.Function.fromMethod(.@"struct", method, @"struct".module),
                } else return utils.printErr(
                    error.UndeclaredProperty,
                    "comperr: '{f}' has no property '{s}' ({f})\n",
                    .{ parent_type, member.member_name, member.parent.getPosition() },
                    .red,
                );
            },
            .@"union" => |@"union"| if (@"union".getProperty(member.member_name)) |property| switch (property) {
                .variable => |v| return v.type,
                .member => |member_type| return member_type.*,
                .method => |method| return Type.Function.fromMethod(.@"struct", method, @"union".module),
            } else return errors.undeclaredProperty(parent_type, member.member_name, member.pos),
            .@"enum" => |@"enum"| if (@"enum".getProperty(member.member_name)) |property| switch (property) {
                .variable => |variable| return variable.type,
                .member => return .{ .@"enum" = @"enum" },
                .method => |method| return Type.Function.fromMethod(.@"enum", method, @"enum".module),
            } else return utils.printErr(
                error.UndeclaredProperty,
                "comperr: Enum '{s}' has no property '{s}' ({f})\n",
                .{ @"enum".name, member.member_name, member.pos },
                .red,
            ),
            .reference => |reference| continue :b reference.inner.*,
            .module => |module| if (module.symbols.get(member.member_name)) |symbol| {
                return symbol.type;
            } else return utils.printErr(
                error.UndeclaredProperty,
                "comperr: Module '{s}' has no member '{s}' ({f})\n",
                .{ module.name, member.member_name, member.pos },
                .red,
            ),
            .slice => |slice| return if (std.mem.eql(u8, member.member_name, "len"))
                .usize
            else if (std.mem.eql(u8, member.member_name, "ptr"))
                .{ .reference = .{ .is_mut = slice.is_mut, .inner = slice.inner } }
            else
                utils.printErr(
                    error.UndeclaredProperty,
                    "comperr: Slice type only has members `ptr` and `len`, attempted to use member '{s}' ({f}).\n",
                    .{ member.member_name, member.pos },
                    .red,
                ),
            .type => |t| if (t) |inner|
                continue :b inner.*
            else
                return errors.illegalMemberExpression(.{ .type = null }, member.pos),
            else => |other| return errors.illegalMemberExpression(other, member.pos),
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
    pub fn check(received: Type, expected: Type) bool {
        if (expected == .any) return true;

        if (received.eql(expected)) return true;

        const fallback = switch (expected) {
            .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .usize => switch (received) {
                .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .usize => true,
                else => false,
            },
            .f32, .f64 => switch (received) {
                .f32, .f64, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .usize => true,
                else => false,
            },
            .optional => |inner| inner.check(received),
            .error_union => |error_union| received.check(error_union.success.*) or
                received.check(error_union.failure.*),
            else => received.eql(expected),
        };

        return switch (received) {
            .@"typeof(undefined)" => true,
            .@"typeof(null)" => expected == .optional,
            .reference => |received_ref| switch (expected) {
                .reference => |expected_ref| received_ref.inner.check(expected_ref.inner.*) and
                    (received_ref.is_mut or !expected_ref.is_mut),
                else => fallback,
            },
            else => fallback,
        };
    }

    pub fn format(
        self: *const Self,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self.*) {
            inline .@"struct", .@"enum", .@"union" => |compound| {
                if (compound.generic_instantiation) |inst| {
                    try writer.print("{s}<", .{inst.base_name});
                    for (inst.args, 0..) |arg, i| {
                        try writer.print("{f}", .{arg});
                        if (i < inst.args.len - 1) _ = try writer.write(", ");
                    }
                    _ = try writer.write(">");
                } else {
                    _ = try writer.write(compound.name);
                }
            },
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
            .slice => |slice| {
                _ = try writer.write("[");
                _ = try writer.write("]");
                if (slice.is_mut) _ = try writer.write("mut ");
                try writer.print("{f}", .{slice.inner.*});
            },
            .error_union => |error_union| {
                try writer.print("{f}", .{error_union.failure});
                try writer.print("!{f}", .{error_union.success});
            },
            .function => |function| {
                _ = try writer.write("fn (");
                for (function.params.items, 1..) |param, i| {
                    try writer.print("{f}", .{param.type});
                    if (i < function.params.items.len) _ = try writer.write(", ");
                }
                try writer.print(") {f}", .{function.return_type});
            },
            .module => |module| try writer.print("module {s}", .{module.name}),
            .variadic => _ = try writer.write("..."),
            .generic_param => |name| _ = try writer.write(name),
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

                .slice => |r| {
                    h.update(std.mem.asBytes(&ctx.hash(r.inner.*)));
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
                    for (f.params.items) |p| h.update(std.mem.asBytes(&ctx.hash(p.type)));
                    for (f.generic_params.items) |p| h.update(std.mem.asBytes(&ctx.hash(p.type)));
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

                        for (entry.value_ptr.params.items) |p|
                            eh.update(std.mem.asBytes(&ctx.hash(p.type)));
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

                        for (entry.value_ptr.params.items) |p|
                            eh.update(std.mem.asBytes(&ctx.hash(p.type)));

                        eh.update(std.mem.asBytes(&ctx.hash(entry.value_ptr.return_type.*)));

                        const mixed = eh.final();
                        h.update(std.mem.asBytes(&mixed));
                    }
                },

                else => {}, // all primitives // TODO: some new non-primitive types might be getting caught into the else block
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
                            if (!ctx.eql(a_method.params.items[i].type, b_method.params.items[i].type))
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
                            if (!ctx.eql(a_method.params.items[i].type, b_method.params.items[i].type)) return false;
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
                        if (!ctx.eql(ta.params.items[i].type, b.function.params.items[i].type)) return false;
                    }

                    return ctx.eql(ta.return_type.*, b.function.return_type.*);
                },

                else => true, // primitive types
            };
        }
    };

    /// returns an unsigned integer type given a length
    pub fn uintFromBits(bits: u6) Type {
        return if (bits <= 8)
            .u8
        else if (bits <= 16)
            .u16
        else if (bits <= 32)
            .u32
        else if (bits <= 64)
            .u64
        else
            unreachable;
    }

    pub fn getTagType(enum_length: usize) Type {
        return uintFromBits(@intFromFloat(@ceil(std.math.log2(@as(f32, @floatFromInt(enum_length))))));
    }

    pub fn isNumeric(self: Type) bool {
        return switch (self) {
            .u64, .u32, .u16, .u8 => true,
            .i64, .i32, .i16, .i8 => true,
            .f64, .f32 => true,
            .usize => true,
            else => false,
        };
    }
};
