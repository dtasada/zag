const std = @import("std");

const utils = @import("utils");
const ast = @import("ast");

const statements = @import("../statements.zig");
const expressions = @import("../expressions.zig");
const errors = @import("../errors.zig");
const types = @import("types.zig");

const Value = @import("../Value.zig").Value;
const Compiler = @import("../Compiler.zig");
const Module = @import("../Module.zig");
const CompilerError = errors.CompilerError;

pub const Type = union(enum) {
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

    c_float,
    c_double,

    usize,

    f32,
    f64,

    bool,

    void,

    type_type,
    type: *const Type,
    any,

    @"typeof(nil)",
    @"typeof(undefined)",
    generic_param: []const u8,

    @"struct": types.Struct,
    @"enum": types.Enum,
    @"union": types.Union,
    optional: *const Type,
    slice: types.Slice,
    reference: types.Reference,
    array: types.Array,
    error_union: types.ErrorUnion,
    function: types.Function,
    module: Module,
    variadic,

    pub const Function = types.Function;
    pub const Struct = types.Struct;
    pub const Union = types.Union;
    pub const Enum = types.Enum;
    pub const CompoundType = types.CompoundType;
    pub const Subtype = types.Subtype;
    pub const fromCompoundTypeDeclaration = types.fromCompoundTypeDeclaration;

    /// Converts an AST type to a Compiler type.
    /// `infer` is the expression with which the type is inferred.
    pub fn fromAst(compiler: *Compiler, t: ast.Type) CompilerError!Type {
        return switch (t) {
            .symbol => |symbol| {
                const result = compiler.getSymbolType(symbol.inner) catch
                    return errors.unknownSymbol(symbol.inner, symbol.pos);
                if (result == .void and !std.mem.eql(u8, symbol.inner, "void"))
                    return errors.unknownSymbol(symbol.inner, symbol.pos);
                // If we get a .type, unwrap it to the actual type
                if (result == .type) return result.type.*;
                return result;
            },
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
                    try compiler.pushScope(false);
                    defer compiler.popScope();

                    const generic_params = try compiler.alloc.alloc(types.Function.Param, function.generic_parameters.len);
                    for (function.generic_parameters, 0..) |p, i| {
                        generic_params[i] = .{
                            .name = p.name,
                            .type = if (p.type == .inferred) .type_type else try .fromAst(compiler, p.type),
                        };
                        try compiler.registerSymbol(p.name, .{ .type = .{ .generic_param = p.name } }, .{});
                    }

                    const params = try compiler.alloc.alloc(types.Function.Param, function.parameters.len);
                    for (function.parameters, 0..) |p, i| params[i] = .{
                        .name = p.name,
                        .type = try fromAst(compiler, p.type),
                    };

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
                    failure.* = if (error_union.failure) |err| try fromAst(compiler, err.*) else .void;

                    break :b .{ .success = success, .failure = failure };
                },
            },
            .inferred => unreachable,
            .member => |member| b: switch (try fromAst(compiler, member.parent.*)) {
                inline .@"struct", .@"union" => |s, tag| if (s.subtypes.get(member.member_name)) |subtype_wrapper| {
                    const ptr = try compiler.alloc.create(Type);
                    ptr.* = switch (subtype_wrapper.type) {
                        .@"struct" => |p| .{ .@"struct" = p },
                        .@"union" => |p| .{ .@"union" = p },
                        .@"enum" => |p| .{ .@"enum" = p },
                    };
                    return .{ .type = ptr };
                } else if (s.variables.get(member.member_name)) |v|
                    v.type
                else
                    errors.undeclaredProperty(@unionInit(Type, @tagName(tag), s), member.member_name, member.pos),
                .@"enum" => |e| if (e.getProperty(member.member_name)) |property| switch (property) {
                    .variable => |v| v.type,
                    .member => .{ .@"enum" = e },
                    .method => |method| Type.Function.fromMethod(method, e.module),
                    .subtype => |subtype| subtype.toType(),
                } else errors.undeclaredProperty(.{ .@"enum" = e }, member.member_name, member.pos),
                .module => |module| if (module.symbols.get(member.member_name)) |symbol| b2: {
                    if (!symbol.is_pub) break :b2 errors.badAccess(module.name, member.member_name, member.pos);
                    break :b symbol.type;
                } else errors.undeclaredProperty(.{ .module = module }, member.member_name, member.pos),
                .reference => |ref| continue :b ref.inner.*,
                .type => |type_ptr| continue :b type_ptr.*,
                .type_type => errors.illegalMemberExpression(.type_type, member.pos),
                else => |other| errors.illegalMemberExpression(other, member.pos),
            },
        };
    }

    pub fn fromAstPtr(compiler: *Compiler, t: ast.Type) CompilerError!*Type {
        const ptr = try compiler.alloc.create(Type);
        ptr.* = try fromAst(compiler, t);
        return ptr;
    }

    fn instantiateBuiltin(
        compiler: *Compiler,
        base_function: Type.Function,
        name: []const u8,
        base_name: []const u8,
        args: []const Value,
        comptime modify_return_type: bool,
    ) !Type {
        var new_f = base_function;
        new_f.name = name;
        new_f.generic_params = &.{};
        new_f.generic_instantiation = .{
            .base_name = base_name,
            .args = try compiler.alloc.dupe(Value, args),
        };

        if (modify_return_type and args.len > 0) {
            switch (args[0]) {
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
        try compiler.scopes.items[0].items.put(name, .{ .type = .{
            .type = new_type,
            .inner_name = name,
            .is_defined = true,
            .should_free = false,
        } });

        return new_type;
    }

    fn instantiateGeneric(
        compiler: *Compiler,
        base_type: Type,
        arguments: ast.ArgumentList,
        pos: utils.Position,
    ) CompilerError!Type {
        const args = try compiler.alloc.alloc(Value, arguments.len);
        defer {
            for (args) |v| v.deinit(compiler.alloc);
            compiler.alloc.free(args);
        }
        for (arguments, 0..) |arg, i| args[i] = try compiler.solveComptimeExpression(arg);

        // Check generic params
        const params = switch (base_type) {
            inline .@"struct", .@"union", .function => |s| s.generic_params,
            else => return utils.printErr(
                error.TypeNotGeneric,
                "comperr: Type '{f}' is not generic ({f})\n",
                .{ base_type, pos },
                .red,
            ),
        };

        if (params.len != args.len)
            return errors.genericArgumentCountMismatch(params.len, args.len, pos);

        // Mangle name
        const base_name = switch (base_type) {
            inline .@"struct", .@"union", .function => |s| s.name,
            else => unreachable,
        };

        // If the arguments are identical to the base type's own generic parameters,
        // this is a self-reference to the generic template (e.g. ArrayList<T> inside ArrayList<T>).
        // In this case, just return the base type.
        var all_match = true;
        for (params, 0..) |p, i| {
            const arg = args[i];
            if (arg != .type or arg.type != .generic_param or
                !std.mem.eql(u8, arg.type.generic_param, p.name))
            {
                all_match = false;
                break;
            }
        }
        if (all_match) return base_type;

        var name: std.ArrayList(u8) = .empty;
        defer name.deinit(compiler.alloc);

        try name.appendSlice(compiler.alloc, base_name);
        for (args) |arg| try name.print(compiler.alloc, "_{x}", .{arg.hash()});

        // Check if already instantiated
        if (compiler.getSymbolType(name.items)) |t| return t else |_| {}

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
            try compiler.pushScope(false);

            var mod_it = module.symbols.iterator();
            while (mod_it.next()) |entry| {
                const sym = entry.value_ptr.*;
                const scope_item: Compiler.ScopeItem = switch (sym.type) {
                    .@"enum", .@"struct", .@"union" => .{ .type = .{
                        .type = sym.type,
                        .inner_name = sym.inner_name,
                        .is_defined = true,
                        .should_free = false,
                    } },
                    else => .{ .symbol = .{
                        .type = sym.type,
                        .inner_name = sym.inner_name,
                        .is_mut = false,
                        .is_defined = true,
                        .should_free = false,
                    } },
                };
                try compiler.scopes.getLast().items.put(entry.key_ptr.*, scope_item);
            }

            // Then register imports and generic params (may override above)
            var imp_it = module.imports.iterator();
            while (imp_it.next()) |entry|
                try compiler.registerSymbol(entry.key_ptr.*, .{ .module = entry.value_ptr.* }, .{});

            try compiler.registerSymbol(
                base_name,
                .{ .type = base_type },
                .{ .inner_name = try compiler.mangle(base_name) },
            );

            for (params, 0..) |param, i| {
                const val = args[i];
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
                inline .@"struct", .@"union" => |s, tag| blk: {
                    const copy_decl = try compiler.alloc.create(@TypeOf(s.*));
                    copy_decl.* = try s.clone(compiler.alloc);
                    copy_decl.name = try compiler.alloc.dupe(u8, name.items);
                    copy_decl.generic_types = &.{};
                    var t = try types.fromCompoundTypeDeclaration(
                        compiler,
                        std.meta.stringToEnum(utils.CompoundTypeTag, @tagName(tag)).?,
                        copy_decl,
                        .{},
                    );
                    t.module = module;
                    t.generic_instantiation = .{ .base_name = base_name, .args = try utils.cloneSlice(Value, args, compiler.alloc) };
                    break :blk @unionInit(Type, @tagName(tag), t);
                },
                .function => |d| blk: {
                    var copy = try d.clone(compiler.alloc);
                    copy.name = try compiler.alloc.dupe(u8, name.items);
                    copy.generic_parameters = &.{};
                    var t = try fromAst(compiler, copy.getType());
                    t.function.definition = d;
                    t.function.generic_instantiation = .{
                        .base_name = base_name,
                        .args = try utils.cloneSlice(Value, args, compiler.alloc),
                    };
                    break :blk t;
                },
            };

            compiler.popScope(); // Unregisters placeholders

            // Register globally
            const name_copy = try compiler.alloc.dupe(u8, name.items);
            try compiler.scopes.items[0].items.put(name_copy, .{ .type = .{
                .type = new_type,
                .inner_name = switch (new_type) {
                    .@"struct" => new_type.@"struct".inner_name,
                    .@"union" => new_type.@"union".inner_name,
                    .function => new_type.function.inner_name,
                    else => name_copy,
                },
                .should_free = false,
                .is_defined = false,
            } });

            // Add to pending instantiations
            try compiler.pending_instantiations.append(compiler.alloc, .{
                .inner_name = try name.toOwnedSlice(compiler.alloc),
                .args = try utils.cloneSlice(Value, args, compiler.alloc),
                .module = module,
                .t = switch (def_wrap) {
                    .@"struct" => |d| .{ .@"struct" = d.* },
                    .@"union" => |d| .{ .@"union" = d.* },
                    .function => |d| .{ .function = d.* },
                },
            });

            return new_type;
        } else return if (base_type == .function and std.mem.eql(u8, base_type.function.name, "sizeof"))
            instantiateBuiltin(compiler, base_type.function, try name.toOwnedSlice(compiler.alloc), base_name, args, false)
        else if (base_type == .function and (std.mem.eql(u8, base_type.function.name, "cast") or
            std.mem.eql(u8, base_type.function.name, "xor")))
            instantiateBuiltin(compiler, base_type.function, try name.toOwnedSlice(compiler.alloc), base_name, args, true)
        else
            utils.printErr(
                error.GenericInstantiationFailed,
                "comperr: Cannot instantiate {f} (missing definition or unsupported) ({f})\n",
                .{ base_type, pos },
                .red,
            );
    }

    pub fn infer(compiler: *Compiler, expr: ast.Expression) CompilerError!Type {
        return switch (expr) {
            .ident => |ident| switch (compiler.getScopeItem(ident.payload) catch
                return errors.unknownSymbol(ident.payload, expr.getPosition())) {
                .symbol => |s| s.type,
                .type => |*t| .{ .type = &t.type },
                .module => |m| .{ .module = m },
                .constant => |c| c.type,
            },
            .string => .{ .slice = .{ .inner = &.u8, .is_mut = false } },
            .char => .u8,
            .int => |int| if (int.payload <= std.math.maxInt(i32)) .i32 else .i64,
            .float => |float| if (float.payload == @as(f64, @floatCast(@as(f32, @floatCast(float.payload)))))
                // if the float fits in an f32, then default to f32. if the float is too big,
                // use f64
                .f32
            else
                .f64,

            .call => |call| try inferCallExpression(compiler, call),
            .member => |member| try inferMemberExpression(compiler, member),
            .@"try" => |t| {
                const inner_type = try infer(compiler, t.payload.*);

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
                    .{ lhs, @tagName(binary.op), rhs, binary.pos },
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
                for (comp.comparisons) |cmp| {
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
            .struct_instantiation => |struct_inst| b: {
                const inferred_type = try infer(compiler, struct_inst.type_expr.*);
                break :b if (inferred_type == .type) inferred_type.type.* else inferred_type;
            },
            .array_instantiation => |array| try inferArrayInstantiationExpression(compiler, array),
            .block => |block| try inferBlock(compiler, block),
            .@"if" => |@"if"| if (@"if".@"else") |@"else"| {
                // If the condition is comptime-known, only infer the taken branch.
                // This allows the branches to have different types (e.g. comptime type dispatch).
                if (compiler.solveComptimeExpression(@"if".condition.*)) |val| {
                    return switch (val) {
                        .bool => |cond| if (cond)
                            try infer(compiler, @"if".body.*)
                        else
                            try infer(compiler, @"else".*),
                        else => return error.ExpressionCannotBeEvaluatedAtCompileTime,
                    };
                } else |_| {}

                // Runtime if: both branches must have compatible types.
                try compiler.pushScope(false);
                if (@"if".capture) |capture| {
                    const capture_type = switch (try infer(compiler, @"if".condition.*)) {
                        .optional => |optional| optional.*,
                        else => |other| other,
                    };
                    try compiler.registerSymbol(capture.name, .{ .symbol = .{ .type = capture_type } }, .{});
                }

                const expected: Type = try infer(compiler, @"if".body.*);
                compiler.popScope();

                const received: Type = try infer(compiler, @"else".*);
                if (!received.check(expected))
                    return errors.typeMismatchIfExpression(expected, received, @"if".pos);

                if (expected == .@"typeof(nil)" or received == .@"typeof(nil)") {
                    const value_type = if (received == .@"typeof(nil)") expected else received;
                    const inner = try compiler.alloc.create(Type);
                    inner.* = value_type;
                    return .{ .optional = inner };
                }

                return expected;
            } else return errors.ifExpressionMustContainElseClause(@"if".pos),
            .index => |index| switch (try infer(compiler, index.lhs.*)) {
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
                while (base_type == .type) {
                    if (base_type.type.* != .type_type) base_type = base_type.type.* else break;
                }

                const t = try compiler.alloc.create(Type);
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
            .match => |m| {
                var result_type: ?Type = null;
                for (m.cases) |case| {
                    result_type = if (case.result == .expression)
                        try .infer(compiler, case.result.expression)
                    else if (result_type) |rt|
                        return errors.typeMismatchMatchExpression(rt, .void, case.pos)
                    else
                        .void;
                }
                return result_type.?;
            },
            .type => |t| .{ .type = try fromAstPtr(compiler, t.payload) },
            .slice => |slice| switch (try infer(compiler, slice.lhs.*)) {
                .slice => |s| .{ .slice = s },
                .array => |a| .{ .slice = .{ .inner = a.inner, .is_mut = try compiler.getExpressionMutability(expr) } },
                else => |other| errors.illegalSliceExpression(other, slice.pos),
            },
            .bad_node => unreachable,
            .@"catch" => |c| {
                const lhs_t = try infer(compiler, c.lhs.*);
                const inner = switch (lhs_t) {
                    .error_union => |eu| eu.success.*,
                    else => return errors.catchExpressionOnNonErrorUnion(lhs_t, c.pos),
                };

                const rhs_t = try infer(compiler, c.rhs.*);

                if (!(inner.eql(rhs_t) or inner.check(rhs_t) or inner.check(lhs_t)))
                    return errors.typeMismatchCatchExpression(lhs_t, rhs_t, c.pos);

                return inner;
            },
            .dereference => |deref| switch (try infer(compiler, deref.parent.*)) {
                .reference => |ref| ref.inner.*,
                else => |other| utils.printErr(
                    error.IllegalExpression,
                    "comperr: Invalid dereference on expression of type '{f}' ({f}).\n",
                    .{ other, deref.pos },
                    .red,
                ),
            },
        };
    }

    fn inferCallExpression(compiler: *Compiler, call: ast.Expression.Call) !Type {
        const callee_type: Type = try infer(compiler, call.callee.*);

        return switch (callee_type) {
            .function => |function| function.return_type.*,
            else => |t| errors.illegalCallExpression(t, call.pos),
        };
    }

    pub fn inferBlock(self: *Compiler, blk: ast.Expression.Block) !Type {
        try self.pushScope(false);
        defer self.popScope();

        for (blk.payload) |statement| switch (statement) {
            .variable_definition => |vd| try self.registerSymbol(vd.variable_name, .{
                .symbol = .{ .type = try .infer(self, vd.assigned_value) },
            }, .{}),
            .block_eval => |expr| return try .infer(self, expr),
            else => {},
        };

        return .void;
    }

    fn inferArrayInstantiationExpression(compiler: *Compiler, array: ast.Expression.ArrayInstantiation) !Type {
        const t = try fromAstPtr(compiler, array.type);

        const size = if (array.length.* == .ident and std.mem.eql(u8, array.length.ident.payload, "_"))
            array.contents.len
        else b: {
            const expected_length = (try compiler.solveComptimeExpression(array.length.*)).u64;

            const received_length = array.contents.len;
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

    fn inferMemberExpression(compiler: *Compiler, member: ast.Expression.Member) !Type {
        const parent_type = try infer(compiler, member.parent.*);
        b: switch (parent_type) {
            inline .@"struct", .@"enum", .@"union" => |s, t| return if (s.getProperty(member.member_name)) |property| switch (property) {
                .variable => |v| v.type,
                .member => |m| if (t == .@"enum") .{ .@"enum" = s } else m,
                .method => |method| Type.Function.fromMethod(method, s.module),
                .subtype => |st| st.toType(),
            } else utils.printErr(
                error.UndeclaredProperty,
                "comperr: '{f}' has no property '{s}' ({f})\n",
                .{ parent_type, member.member_name, member.parent.getPosition() },
                .red,
            ),
            .reference => |reference| continue :b reference.inner.*,
            .module => |module| if (module.symbols.get(member.member_name)) |symbol| {
                if (!symbol.is_pub) return errors.badAccess(module.name, member.member_name, member.pos);
                return switch (symbol.type) {
                    .@"struct", .@"union", .@"enum" => wrap: {
                        const ptr = try compiler.alloc.create(Type);
                        ptr.* = symbol.type;
                        break :wrap .{ .type = ptr };
                    },
                    else => symbol.type,
                };
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
            .type => |t| continue :b t.*,
            .type_type => return errors.illegalMemberExpression(.type_type, member.pos),
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
        if (expected == .any or received.eql(expected)) return true;

        const fallback = switch (expected) {
            .optional => |inner| received.check(inner.*),
            .error_union => |error_union| received.check(error_union.success.*) or
                received.check(error_union.failure.*),
            else => expected.isNumeric() and received.isNumeric() or received.eql(expected),
        };

        return switch (received) {
            .@"typeof(undefined)" => true,
            .@"typeof(nil)" => true,
            .optional => |ro| switch (expected) {
                .optional => |eo| ro.check(eo.*),
                else => fallback,
            },
            .slice => |rs| switch (expected) {
                .reference => |er| rs.inner.* == .u8 and er.inner.* == .c_char,
                .slice => |es| rs.inner.check(es.inner.*) and (rs.is_mut or !es.is_mut),
                else => fallback,
            },
            .reference => |received_ref| switch (expected) {
                .optional => |expected_opt| received.check(expected_opt.*),
                .reference => |expected_ref| received_ref.inner.check(expected_ref.inner.*) and
                    (received_ref.is_mut or !expected_ref.is_mut) or
                    expected_ref.inner.* == .void or received_ref.inner.* == .void,
                .slice => |slc| received_ref.inner.* == .array and
                    received_ref.inner.array.inner.check(slc.inner.*),
                else => fallback,
            },
            else => fallback,
        };
    }

    pub fn format(
        self: *const Type,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self.*) {
            inline .@"struct", .@"enum", .@"union" => |compound| {
                if (compound.generic_instantiation) |inst| {
                    try writer.print("{s}.", .{compound.module.name});
                    try writer.print("{s}<", .{inst.base_name});
                    for (inst.args, 0..) |arg, i| {
                        try writer.print("{f}", .{arg});
                        if (i < inst.args.len - 1) _ = try writer.write(", ");
                    }
                    _ = try writer.write(">");
                } else {
                    try writer.print("{s}.", .{compound.module.name});
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
                for (function.params, 1..) |param, i| {
                    try writer.print("{f}", .{param.type});
                    if (i < function.params.len) _ = try writer.write(", ");
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
                    for (f.params) |p| h.update(std.mem.asBytes(&ctx.hash(p.type)));
                    for (f.generic_params) |p| h.update(std.mem.asBytes(&ctx.hash(p.type)));
                    h.update(std.mem.asBytes(&ctx.hash(f.return_type.*)));
                },
                inline .@"struct", .@"union" => |ct| {
                    for (ctx.visited.items) |visit| {
                        if (visit.@"0" == @as(*const anyopaque, @ptrCast(ct.members))) {
                            // Recursive type detected. Just hash the name to avoid infinite loop.
                            h.update(ct.name);
                            return h.final();
                        }
                    }

                    // No risk of `append` failing because `visited` is a fixed-size buffer
                    // and we'd stack overflow before we fill it.
                    ctx.visited.appendAssumeCapacity(.{ @as(*const anyopaque, @ptrCast(ct.members)), @as(*const anyopaque, @ptrCast(ct.members)) });
                    defer _ = ctx.visited.pop();

                    h.update(ct.inner_name);

                    // members (order-independent)
                    var mit = ct.members.iterator();
                    while (mit.next()) |entry| {
                        var eh = std.hash.Wyhash.init(0);
                        eh.update(entry.key_ptr.*);
                        switch (t) {
                            inline .@"struct", .@"union" => eh.update(std.mem.asBytes(&ctx.hash(entry.value_ptr.*))),
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

                        for (entry.value_ptr.params) |p| eh.update(std.mem.asBytes(&ctx.hash(p.type)));
                        eh.update(std.mem.asBytes(&ctx.hash(entry.value_ptr.return_type.*)));

                        const mixed = eh.final();
                        h.update(std.mem.asBytes(&mixed));
                    }
                },
                .@"enum" => |ct| {
                    for (ctx.visited.items) |visit| {
                        if (visit.@"0" == @as(*const anyopaque, @ptrCast(ct.members))) {
                            // Recursive type detected. Just hash the name to avoid infinite loop.
                            h.update(ct.name);
                            return h.final();
                        }
                    }

                    // No risk of `append` failing because `visited` is a fixed-size buffer
                    // and we'd stack overflow before we fill it.
                    ctx.visited.appendAssumeCapacity(.{ @as(*const anyopaque, @ptrCast(ct.members)), @as(*const anyopaque, @ptrCast(ct.members)) });
                    defer _ = ctx.visited.pop();

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

                        for (entry.value_ptr.params) |p|
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
                inline .@"struct", .@"union" => |ta, tag| {
                    const tb = if (std.meta.activeTag(b) == tag) @field(b, @tagName(tag)) else return false;
                    if (!std.mem.eql(u8, ta.inner_name, tb.inner_name)) return false;

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
                                a_member,
                                t.members.get(name) orelse return false,
                            )) return false,
                            else => unreachable,
                        }
                    }

                    var methods = ta.methods.iterator();
                    while (methods.next()) |entry| {
                        const name = entry.key_ptr.*;
                        const a_method = entry.value_ptr.*;
                        const b_method = b_methods.get(name) orelse return false;

                        for (0..a_method.params.len) |i|
                            if (!ctx.eql(a_method.params[i].type, b_method.params[i].type))
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

                        for (0..a_method.params.len) |i| {
                            if (!ctx.eql(a_method.params[i].type, b_method.params[i].type)) return false;
                        }

                        if (!ctx.eql(a_method.return_type.*, b_method.return_type.*)) return false;
                    }

                    return true;
                },
                .optional => |ta| ctx.eql(ta.*, b.optional.*),
                .reference => |ta| ctx.eql(ta.inner.*, b.reference.inner.*) and ta.is_mut == b.reference.is_mut,
                .slice => |ta| ctx.eql(ta.inner.*, b.slice.inner.*) and ta.is_mut == b.slice.is_mut,
                .array => |ta| ctx.eql(ta.inner.*, b.array.inner.*) and ta.size == b.array.size,
                .error_union => |ta| ctx.eql(ta.success.*, b.error_union.success.*) and
                    ctx.eql(ta.failure.*, b.error_union.failure.*),
                .function => |ta| {
                    if (ta.params.len != b.function.params.len) return false;

                    for (0..ta.params.len) |i| {
                        if (!ctx.eql(ta.params[i].type, b.function.params[i].type)) return false;
                    }

                    return ctx.eql(ta.return_type.*, b.function.return_type.*);
                },

                else => true, // primitive types
            };
        }
    };

    /// returns an unsigned integer type given a length
    pub fn uintFromBits(bits: u7) Type {
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

            .c_char, .c_long, .c_short, .c_int => true,
            .c_uchar, .c_ulong, .c_ushort, .c_uint => true,
            .c_float, .c_double => true,

            else => false,
        };
    }

    fn deinitPtr(self: *const Type, alloc: std.mem.Allocator) void {
        self.deinit(alloc);
        alloc.destroy(self);
    }

    pub fn deinit(self: Type, alloc: std.mem.Allocator) void {
        switch (self) {
            inline .type, .optional => |t| t.deinitPtr(alloc),
            // .generic_param => |gp| alloc.free(gp),
            inline .slice, .reference, .array => |ref| ref.inner.deinitPtr(alloc),
            .error_union => |eu| {
                eu.failure.deinitPtr(alloc);
                eu.success.deinitPtr(alloc);
            },
            .function => |f| {
                alloc.free(f.inner_name);
                for (f.params) |p| p.type.deinit(alloc);
                for (f.generic_params) |p| p.type.deinit(alloc);
                f.return_type.deinitPtr(alloc);
                if (f.definition) |def| {
                    for (def.parameters) |param| param.type.deinit(alloc);
                    for (def.generic_parameters) |param| param.type.deinit(alloc);
                    def.return_type.deinit(alloc);
                }
                if (f.generic_instantiation) |gi| utils.deinitSlice(Value, gi.args, alloc);
            },
            inline .@"struct", .@"union", .@"enum" => |ct, t| {
                // alloc.free(ct.name);
                alloc.free(ct.inner_name);

                var vars_it = ct.variables.valueIterator();
                while (vars_it.next()) |variable| {
                    variable.type.deinit(alloc);
                    alloc.free(variable.inner_name);
                    variable.value.deinit(alloc);
                }
                ct.variables.deinit();
                alloc.destroy(ct.variables);

                var subtypes_it = ct.subtypes.valueIterator();
                while (subtypes_it.next()) |st| alloc.free(st.inner_name);
                ct.subtypes.deinit();
                alloc.destroy(ct.subtypes);

                if (t != .@"enum") {
                    var members_it = ct.members.iterator();
                    while (members_it.next()) |member| member.value_ptr.deinit(alloc);
                }
                ct.members.deinit();
                alloc.destroy(ct.members);

                var methods_it = ct.methods.iterator();
                while (methods_it.next()) |entry| {
                    const method = entry.value_ptr.*;
                    alloc.free(method.inner_name);
                    for (method.params) |param| param.type.deinit(alloc);
                    for (method.generic_params) |param| param.type.deinit(alloc);
                    method.return_type.deinitPtr(alloc);
                    if (method.definition) |def| {
                        for (def.parameters) |param| param.type.deinit(alloc);
                        for (def.generic_parameters) |param| param.type.deinit(alloc);
                        def.return_type.deinit(alloc);
                    }
                }

                for (ct.generic_params) |param| param.type.deinit(alloc);
                if (ct.tag_type) |tt| tt.deinitPtr(alloc);
                if (ct.generic_instantiation != null) {
                    if (ct.definition) |def| alloc.destroy(def);
                }
                if (ct.generic_instantiation) |gi| utils.deinitSlice(Value, gi.args, alloc);
            },
            else => {},
        }
    }

    pub fn clonePtr(self: *const Type, alloc: std.mem.Allocator) !*Type {
        const ret = try alloc.create(Type);
        ret.* = try self.clone(alloc);
        return ret;
    }

    pub fn clone(self: Type, alloc: std.mem.Allocator) std.mem.Allocator.Error!Type {
        return switch (self) {
            .type => |t| .{ .type = try t.clonePtr(alloc) },
            .optional => |opt| .{ .optional = try opt.clonePtr(alloc) },
            .slice => |slice| .{ .slice = .{ .inner = try slice.inner.clonePtr(alloc), .is_mut = slice.is_mut } },
            .reference => |ref| .{ .reference = .{ .inner = try ref.inner.clonePtr(alloc), .is_mut = ref.is_mut } },
            .array => |array| .{ .array = .{ .inner = try array.inner.clonePtr(alloc), .size = array.size } },
            .error_union => |eu| .{ .error_union = .{ .failure = try eu.failure.clonePtr(alloc), .success = try eu.success.clonePtr(alloc) } },
            .function => |f| .{
                .function = .{
                    .name = f.name,
                    .inner_name = try alloc.dupe(u8, f.inner_name),
                    .params = try utils.cloneSlice(types.Function.Param, f.params, alloc),
                    .generic_params = try utils.cloneSlice(types.Function.Param, f.generic_params, alloc),
                    .return_type = try f.return_type.clonePtr(alloc),
                    .definition = if (f.definition) |def| b: {
                        const d = try alloc.create(ast.Statement.FunctionDefinition);
                        d.* = try def.clone(alloc);
                        break :b d;
                    } else null,
                    .module = self.module,
                    .generic_instantiation = if (f.generic_instantiation) |gi| try gi.clone(alloc) else null,
                },
            },
            inline .@"struct", .@"union", .@"enum" => |ct, t| @unionInit(Type, @tagName(t), .{
                .name = ct.name,
                .inner_name = try alloc.dupe(u8, ct.inner_name),
                .variables = b: {
                    const variables = try alloc.create(std.StringHashMap(types.Variable));
                    variables.* = .init(alloc);
                    var vars_it = ct.variables.iterator();
                    while (vars_it.next()) |entry| try variables.put(entry.key_ptr.*, try entry.value_ptr.clone(alloc));
                    break :b variables;
                },
                .subtypes = b: {
                    const subtypes = try alloc.create(std.StringHashMap(types.Subtype));
                    subtypes.* = .init(alloc);
                    var subtypes_it = ct.subtypes.iterator();
                    while (subtypes_it.next()) |entry| try subtypes.put(entry.key_ptr.*, try entry.value_ptr.clone(alloc));
                    break :b subtypes;
                },
                .members = b: {
                    const members = try alloc.create(std.StringHashMap(@TypeOf(ct).MemberType));
                    members.* = .init(alloc);
                    var members_it = ct.members.iterator();
                    while (members_it.next()) |entry| try members.put(
                        entry.key_ptr.*,
                        if (t == .@"enum") entry.value_ptr.* else try entry.value_ptr.clone(alloc),
                    );
                    break :b members;
                },
                .methods = b: {
                    const methods = try alloc.create(std.StringHashMap(types.Method));
                    methods.* = .init(alloc);
                    var methods_it = ct.methods.iterator();
                    while (methods_it.next()) |entry| try methods.put(
                        entry.key_ptr.*,
                        try entry.value_ptr.clone(alloc),
                    );
                    break :b methods;
                },
                .generic_params = try utils.cloneSlice(types.Function.Param, ct.generic_params, alloc),
                .tag_type = if (ct.tag_type) |tt| try tt.clonePtr(alloc) else null,
                .definition = if (ct.definition) |def| b: {
                    const d = try alloc.create(@TypeOf(def.*));
                    d.* = try def.clone(alloc);
                    break :b d;
                } else null,
                .module = ct.module,
                .generic_instantiation = if (ct.generic_instantiation) |gi| try gi.clone(alloc) else null,
            }),
            else => self,
        };
    }
};
