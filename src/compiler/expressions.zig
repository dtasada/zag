const std = @import("std");

const utils = @import("utils");

const ast = @import("Parser").ast;
const errors = @import("errors.zig");
const statements = @import("statements.zig");
const hash = @import("Parser").hash;

const Self = @import("Compiler.zig");
const Value = @import("Value.zig").Value;
const Type = @import("Type.zig").Type;

const CompilerError = errors.CompilerError;

const Opts = struct {
    binding_mut: bool = false,
    expected_type: ?Type = null, // null means infer the type from the expression
    is_variable_declaration: bool = false,
    is_assignment: bool = false,
};
pub fn compile(
    self: *Self,
    expression: *const ast.Expression,
    opts: Opts,
) CompilerError!void {
    if (opts.expected_type) |expected_type| {
        try compileExpected(self, expression, expected_type, opts);
        return;
    }

    switch (expression.*) {
        .assignment => |a| try assignment(self, a),
        .block => |blk| try block(self, blk),
        .binary => |b| try binary(self, b),
        .comparison => |comp| try comparison(self, comp),
        inline .int, .float => |c| try self.print("{}", .{c.payload}),
        .string => |string| {
            try self.write("(");
            try self.compileType(.{ .slice = .{ .inner = &.u8, .is_mut = false } }, .{});
            try self.print("){{ .ptr = (const uint8_t*)\"{s}\", .len = {} }}", .{
                string.payload,
                string.payload.len,
            });
        },
        .char => |char| try self.print("'{c}'", .{char.payload}),
        .call => |c| try call(self, c),
        .member => |m| try member(self, m),
        .range => |range| return utils.printErr(
            error.IllegalExpression,
            "comperr: Illegal range expression at {f}.\n",
            .{range.pos},
            .red,
        ),
        .prefix => |prefix| {
            const t = try Type.infer(self, prefix.rhs.*);
            const valid = switch (prefix.op) {
                .@"-" => t.isNumeric(),
                .@"!" => t == .bool,
            };

            if (valid) {
                try self.write(@tagName(prefix.op));
                try self.write("(");
                try compile(self, prefix.rhs, .{});
                try self.write(")");
            } else return errors.illegalPrefixExpression(prefix.op, t, prefix.pos);
        },
        .ident => |ident| if (self.getInnerName(ident.payload)) |inner_name| {
            try self.write(inner_name);
        } else |_| return errors.unknownSymbol(ident.payload, expression.getPosition()),
        .struct_instantiation => |struct_inst| b: switch (try Type.infer(self, struct_inst.type_expr.*)) {
            .@"struct" => |s| try structInstantiation(self, struct_inst, s),
            .@"union" => |u| try unionInstantiation(self, struct_inst, u),
            .type => |t| continue :b t.*,
            else => unreachable,
        },
        .reference => |reference| {
            switch (reference.inner.*) {
                .ident => |ident| if (reference.is_mut and !try self.getSymbolMutability(ident.payload))
                    return utils.printErr(
                        error.BadMutability,
                        "comperr: Can't use a mutable reference to immutable binding '{s}' ({f}).\n",
                        .{ ident.payload, reference.pos },
                        .red,
                    ),
                else => {},
            }

            try self.write("&");
            try compile(self, reference.inner, .{});
        },
        .array_instantiation => |array| {
            if (!opts.is_variable_declaration) {
                try self.write("(");
                try self.compileType(try .fromAst(self, array.type), .{ .binding_mut = opts.binding_mut });
                try self.print("[])", .{});
            }
            try self.write("{");
            for (array.contents, 1..) |*item, i| {
                try compile(self, item, .{});
                if (i < array.contents.len) try self.write(", ");
            }
            try self.write("}");
        },
        .index => |idx| try index(self, idx),
        .@"if" => |i| try @"if"(self, i),
        .generic => |g| try generic(self, g),
        .match => |m| try match(self, m),
        .type => |t| try self.compileType(try .fromAst(self, t.payload), .{}),
        .slice => |slc| try slice(self, slc, opts.binding_mut),
        .@"try" => |t| {
            const inner_type: Type = try .infer(self, t.payload.*);
            if (inner_type != .error_union)
                return errors.tryExpressionOnNonErrorUnion(inner_type, t.pos);

            if (self.current_return_type.? != .error_union or !inner_type.check(self.current_return_type.?))
                return errors.tryExpressionBadReturnType(inner_type, self.current_return_type.?, t.pos);

            self.currentSection().pos = self.currentSection().current_statement;

            try self.compileType(try .infer(self, t.payload.*), .{});
            const temp_name = try std.fmt.allocPrint(self.alloc, "_{}", .{std.hash.Wyhash.hash(0, std.mem.asBytes(t.payload))});
            try self.print(" {s} = ", .{temp_name});
            try compile(self, t.payload, .{});
            try self.print(";\nif (!{s}.is_success) return (", .{temp_name});
            try self.compileType(self.current_return_type.?, .{});
            try self.print("){{ .is_success = false, .payload.failure = {s}.payload.failure }};\n", .{temp_name});

            self.currentSection().pos = self.currentWriter().items.len;

            try self.print("{s}.payload.success", .{temp_name});
        },
        .@"catch" => |c| {
            const lhs_t: Type = try .infer(self, c.lhs.*);
            const inner = switch (lhs_t) {
                .error_union => |eu| eu.success.*,
                else => return errors.catchExpressionOnNonErrorUnion(lhs_t, c.pos),
            };

            const rhs_t: Type = try .infer(self, c.rhs.*);

            if (!(inner.eql(rhs_t) or inner.check(rhs_t) or inner.check(lhs_t)))
                return errors.typeMismatchCatchExpression(lhs_t, rhs_t, c.pos);

            const temp_name = try std.fmt.allocPrint(self.alloc, "_{}", .{std.hash.Wyhash.hash(0, std.mem.asBytes(c.lhs))});

            self.currentSection().pos = self.currentSection().current_statement;
            try self.compileType(lhs_t, .{});
            try self.print(" {s} = ", .{temp_name});
            try compile(self, c.lhs, .{});
            try self.write(";\n");
            self.currentSection().pos = self.currentWriter().items.len;

            try self.print("({s}.is_success ? {s}.payload.success : (", .{ temp_name, temp_name });
            try compile(self, c.rhs, .{ .expected_type = inner });
            try self.write("))");
        },
        .dereference => |deref| {
            try self.write("*(");
            try compile(self, deref.parent, .{});
            try self.write(")");
        },
        .bad_node => unreachable,
    }
}

/// Compiles expression with an expected type.
fn compileExpected(
    self: *Self,
    expression: *const ast.Expression,
    expected_type: Type,
    opts: Opts,
) !void {
    const expr_t: Type = try .infer(self, expression.*);
    if (expr_t == .error_union and !expr_t.check(self.current_return_type.?))
        return utils.printErr(
            error.TypeMismatch,
            "comperr: Expression of type '{f}' must be tried or caught ({f}).\n",
            .{ expr_t, expression.getPosition() },
            .red,
        );

    if (!expr_t.check(expected_type))
        return errors.typeMismatch(expected_type, expr_t, expression.getPosition());

    if (expr_t == .reference and expr_t.reference.inner.* == .array and expected_type == .slice and
        (expected_type.slice.is_mut and !try self.getExpressionMutability(expression.*)))
    {
        const err = errors.typeMismatch(expected_type, expr_t, expression.getPosition());
        utils.print("Maybe try using a mutable reference.\n", .{}, .red);
        return err;
    }

    var run_standard = false;
    if (!expected_type.eql(expr_t)) switch (expected_type) {
        .optional => |opt| switch (expr_t) {
            .@"typeof(nil)" => try self.print(
                "({s}){{ .is_some = false }}",
                .{try self.getTypeFromZagHeader(expected_type)},
            ),
            else => if (expr_t.check(opt.*)) {
                try self.print(
                    "({s}){{ .is_some = true, .payload = ",
                    .{try self.getTypeFromZagHeader(expected_type)},
                );
                try compile(self, expression, .{ .expected_type = opt.* });
                try self.write(" }");
            } else return errors.typeMismatch(
                expected_type,
                expr_t,
                expression.getPosition(),
            ),
        },
        .error_union => |error_union| {
            const error_union_type_name = try self.getTypeFromZagHeader(expected_type);

            if (expr_t.check(error_union.success.*)) {
                try self.print("({s}){{ .is_success = true, .payload.success = ", .{error_union_type_name});
                try compile(self, expression, .{});
                try self.write(" }");
            } else if (expr_t.check(error_union.failure.*)) {
                try self.print("({s}){{ .is_success = false, .payload.failure = ", .{error_union_type_name});
                try compile(self, expression, .{});
                try self.write(" }");
            } else if (error_union.success.* == .void and expr_t == .i32) {
                try self.print("({s}){{ .is_success = true, .payload.success = 0 }}", .{error_union_type_name});
            } else unreachable;
        },
        .slice => |slc| if (expr_t == .reference and
            expr_t.reference.inner.* == .array and
            expr_t.reference.inner.array.inner.eql(slc.inner.*))
        {
            try self.write("(");
            try self.compileType(expected_type, .{});
            try self.write("){ .ptr = ");
            try compile(self, expression, .{});
            try self.print(", .len = {} }}", .{expr_t.reference.inner.array.size});
        } else {
            run_standard = true;
        },
        .reference => |ref| if (expr_t == .slice and
            expr_t.slice.inner.* == .u8 and
            ref.inner.* == .c_char) switch (expression.*) {
            .string => |str| try self.print("\"{s}\"", .{str.payload}),
            else => run_standard = true,
        } else {
            run_standard = true;
        },
        else => run_standard = true,
    } else run_standard = true;

    if (run_standard) try compile(self, expression, .{
        .binding_mut = opts.binding_mut,
        .is_variable_declaration = opts.is_variable_declaration,
        .is_assignment = opts.is_assignment,
    });
}

fn @"if"(self: *Self, expr: ast.Expression.If) !void {
    const @"else" = expr.@"else" orelse
        return errors.ifExpressionMustContainElseClause(expr.pos);

    const condition_type: Type = try .infer(self, expr.condition.*);
    // if there's a capture group, it can't be a ternary expression.
    if (expr.capture) |capture| {
        self.currentSection().pos = self.currentSection().current_statement;

        // Infer result type
        try self.pushScope(false);
        const capture_type = switch (condition_type) {
            .optional => |optional| optional.*,
            else => |other| other,
        };
        try self.registerSymbol(capture.name, .{ .symbol = .{ .type = capture_type } }, .{});
        const body_type = try Type.infer(self, expr.body.*);
        self.popScope();

        const else_type = try Type.infer(self, @"else".*);

        if (!body_type.check(else_type) and !else_type.check(body_type))
            return errors.typeMismatchIfExpression(body_type, else_type, expr.pos);

        const opt_type: ?Type = if (body_type == .@"typeof(nil)" or else_type == .@"typeof(nil)") b: {
            const value_type = if (else_type == .@"typeof(nil)") body_type else else_type;
            const inner = try self.alloc.create(Type);
            inner.* = value_type;
            break :b .{ .optional = inner };
        } else null;

        var temp_name: ?[]const u8 = null;
        if (body_type != .void) {
            try self.compileType(body_type, .{ .binding_mut = true });
            temp_name = try std.fmt.allocPrint(self.alloc, "_{}", .{std.hash.Wyhash.hash(0, std.mem.asBytes(&expr))});
            try self.print(" {s};\n", .{temp_name.?});
        }

        // Use a temporary for the condition expression to avoid re-evaluation.
        const cond_var = try std.fmt.allocPrint(self.alloc, "_if_cond_{}", .{std.hash.Wyhash.hash(0, std.mem.asBytes(expr.condition))});
        try self.compileType(condition_type, .{});
        try self.print(" {s} = ", .{cond_var});
        try compile(self, expr.condition, .{ .expected_type = opt_type });
        try self.write(";\n");

        try self.print("if ({s}.is_some) {{\n", .{cond_var});
        try self.pushScope(false);

        try self.registerSymbol(capture.name, .{ .symbol = .{ .type = capture_type } }, .{});
        try self.compileType(capture_type, .{});
        try self.print(" {s} = {s}.payload;\n", .{ capture.name, cond_var });

        if (temp_name) |tn| try self.print("{s} = ", .{tn});
        try compile(self, expr.body, .{ .expected_type = opt_type });
        try self.write(";\n");

        self.popScope();
        try self.write("} else {\n");
        try self.pushScope(false);

        if (temp_name) |tn| try self.print("{s} = ", .{tn});
        try compile(self, @"else", .{ .expected_type = opt_type });
        try self.write(";\n");

        self.popScope();
        try self.write("}\n");

        self.currentSection().pos = self.currentWriter().items.len;

        if (temp_name) |tn| try self.write(tn);
    } else {
        const body_type: Type = try .infer(self, expr.body.*);
        const else_type: Type = try .infer(self, @"else".*);

        if (!body_type.check(else_type) and !else_type.check(body_type))
            return errors.typeMismatchIfExpression(body_type, else_type, expr.pos);

        const opt_type: ?Type = if (body_type == .@"typeof(nil)" or else_type == .@"typeof(nil)") b: {
            const value_type = if (else_type == .@"typeof(nil)") body_type else else_type;
            const inner = try self.alloc.create(Type);
            inner.* = value_type;
            break :b .{ .optional = inner };
        } else null;

        try self.write("((");
        try compile(self, expr.condition, .{});
        try self.write(")");
        if (condition_type == .optional) try self.write(".is_some");

        try self.write(" ? ");
        try compile(self, expr.body, .{ .expected_type = opt_type });
        try self.write(" : ");
        try compile(self, @"else", .{ .expected_type = opt_type });
        try self.write(")");
    }
}

fn block(self: *Self, blk: ast.Expression.Block) !void {
    self.currentSection().pos = self.currentSection().current_statement;

    const block_t: Type = try .inferBlock(self, blk);
    var temp_name: ?[]const u8 = null;
    if (block_t != .void) {
        try self.compileType(block_t, .{ .binding_mut = true });
        temp_name = try std.fmt.allocPrint(self.alloc, "_{}", .{std.hash.Wyhash.hash(0, std.mem.asBytes(&blk))});
        try self.print(" {s};\n", .{temp_name.?});
    }

    try self.pushScope(false);
    try self.write("{\n");
    var received_eval = false;
    for (blk.payload) |*stmt| switch (stmt.*) {
        .block_eval => |*be| {
            if (received_eval) return utils.printErr(
                error.IllegalStatement,
                "comperr: Can only return from a block expression once ({f}).\n",
                .{blk.pos},
                .red,
            );

            try self.print("{s} = ", .{temp_name.?});
            try compile(self, be, .{});
            try self.write(";\n");
            received_eval = true;
        },
        else => try statements.compile(self, stmt),
    };
    try self.write("}\n");
    self.popScope();

    self.currentSection().pos = self.currentWriter().items.len;
    if (temp_name) |tn| try self.write(tn);
}

fn slice(self: *Self, slc: ast.Expression.Slice, binding_mut: bool) !void {
    var t = try Type.infer(self, slc.lhs.*);
    if (t != .slice and t != .array)
        return errors.illegalSliceExpression(t, slc.pos);

    const array_length = if (t == .slice) null else t.array.size;

    const is_slice = t == .slice;
    const is_mut = switch (slc.lhs.*) {
        .ident => |ident| try self.getSymbolMutability(ident.payload),
        else => false,
    };

    if (!is_slice) {
        // leave it like this instead of inlining it because zig is stupid
        const inner = t.array.inner;
        t = .{
            .slice = .{
                .inner = inner,
                .is_mut = is_mut,
            },
        };
    }

    try self.write("(");
    try self.compileType(t, .{ .binding_mut = binding_mut });
    try self.write("){ .ptr = &((");
    try compile(self, slc.lhs, .{});
    try self.write(")");
    if (is_slice) try self.write(".ptr");
    try self.write("[");
    if (slc.start) |start| try compile(self, start, .{}) else try self.write("0");
    try self.write("])");
    try self.write(", .len = ");
    if (slc.inclusive) try self.write("1 + (");

    if (slc.end) |end| {
        try compile(self, end, .{});
    } else if (is_slice) {
        try self.write("(");
        try compile(self, slc.lhs, .{});
        try self.write(").len");
    } else try self.print("{}", .{array_length.?});

    if (slc.inclusive) try self.write(")");

    try self.write(" }");
}

fn index(self: *Self, idx: ast.Expression.Index) !void {
    switch (try Type.infer(self, idx.lhs.*)) {
        .array => |array| {
            if (self.solveComptimeExpression(idx.index.*)) |ce| switch (ce) {
                inline .i64, .u64, .u8 => |int| {
                    if (array.size <= int) return utils.printErr(
                        error.IllegalExpression,
                        "comperr: Tried to index array of length {} with index {} ({f}).\n",
                        .{ array.size, int, idx.pos },
                        .red,
                    );

                    if (int < 0) return utils.printErr(
                        error.IllegalExpression,
                        "comperr: Tried to index array with negative index {} ({f}).\n",
                        .{ int, idx.pos },
                        .red,
                    );
                },
                else => {},
            } else |_| {}

            try compile(self, idx.lhs, .{});
            try self.write("[");
            try compile(self, idx.index, .{});
            try self.write("]");
        },
        .slice => {
            try compile(self, idx.lhs, .{});
            try self.write(".ptr[");
            try compile(self, idx.index, .{});
            try self.write("]");
        },
        else => |other| return utils.printErr(
            error.IllegalExpression,
            "comperr: Illegal index expression on '{f}' ({f}).\n",
            .{ other, idx.lhs.getPosition() },
            .red,
        ),
    }
}

fn generic(self: *Self, g: ast.Expression.Generic) !void {
    const t = try Type.infer(self, .{ .generic = g });

    switch (t) {
        .function => |f| try self.write(f.inner_name),
        else => try self.compileType(t, .{}),
    }
}

fn match(self: *Self, m: ast.Expression.Match) !void {
    self.currentSection().pos = self.currentSection().current_statement;

    const block_t: Type = try .infer(self, .{ .match = m });
    var temp_name: ?[]const u8 = null;
    if (block_t != .void) {
        try self.compileType(block_t, .{ .binding_mut = true });
        temp_name = try std.fmt.allocPrint(self.alloc, "_{}", .{std.hash.Wyhash.hash(0, std.mem.asBytes(&m))});
        try self.print(" {s};\n", .{temp_name.?});
    }

    switch (try Type.infer(self, m.condition.*)) {
        .@"union" => |@"union"| {
            try self.write("switch ((");
            try compile(self, m.condition, .{});
            try self.write(").tag) {\n");

            for (m.cases) |case| {
                switch (case.condition) {
                    .opts => |cases| for (cases) |c| {
                        try self.write("case ");
                        switch (c) {
                            .ident => |ident| {
                                const mem = @"union".getMember(ident.payload) catch return utils.printErr(
                                    error.IllegalExpression,
                                    "comperr: Union type '{s}' doesn't have member '{s}' ({f}).\n",
                                    .{ @"union".name, ident.payload, c.getPosition() },
                                    .red,
                                );
                                try self.print("__zag_{s}_{s}", .{ @"union".tag_type.?.@"enum".inner_name, mem.member_name });
                            },
                            else => return utils.printErr(
                                error.IllegalExpression,
                                "comperr: Match statement case on union type '{s}' must be a member identifier ({f}).\n",
                                .{ @"union".name, c.getPosition() },
                                .red,
                            ),
                        }

                        try self.write(":\n");
                    },
                    .@"else" => try self.write("default:\n"),
                }

                if (temp_name) |tn| try self.print("{s} = ", .{tn});
                try statements.compile(self, &case.result);
                if (temp_name) |_| try self.write(";");
                try self.write("break;\n");
            }

            try self.write("}");
        },
        .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .usize, .c_char, .c_int, .bool => {
            try self.write("switch (");
            try compile(self, m.condition, .{});
            try self.write(") {\n");

            for (m.cases) |case| {
                switch (case.condition) {
                    .opts => |opts| for (opts) |opt| {
                        try self.write("case ");
                        try compile(self, &opt, .{});
                        try self.write(":\n");
                    },
                    .@"else" => try self.write("default:\n"),
                }

                if (temp_name) |tn| try self.print("{s} = ", .{tn});
                try statements.compile(self, &case.result);
                try self.write("break;\n");
            }

            try self.write("}");
        },
        else => |other| return utils.printErr(
            error.IllegalExpression,
            "comperr: Illegal match on expression of type '{f}' ({f}).\n",
            .{ other, m.pos },
            .red,
        ),
    }

    self.currentSection().pos = self.currentWriter().items.len;
    if (block_t != .void) try self.write(temp_name.?);
}

fn structInstantiation(self: *Self, struct_inst: ast.Expression.StructInstantiation, t: Type.Struct) !void {
    // check for missing members
    var struct_members_it = t.members.iterator();
    while (struct_members_it.next()) |struct_member|
        if (struct_inst.members.get(struct_member.key_ptr.*) == null)
            return utils.printErr(
                error.MissingStructMember,
                "comperr: Missing member '{s}' in instantiation of struct '{s}' ({f}).\n",
                .{ struct_member.key_ptr.*, t.name, struct_inst.pos },
                .red,
            );

    // check for extraneous members
    var inst_members_it = struct_inst.members.iterator();
    while (inst_members_it.next()) |inst_member|
        if (t.members.get(inst_member.key_ptr.*) == null)
            return utils.printErr(
                error.ExtraneousStructMember,
                "comperr: Extraneous member '{s}' in instantiation of struct '{s}' ({f}).\n",
                .{ inst_member.key_ptr.*, t.name, struct_inst.pos },
                .red,
            );

    try self.write("(");
    try self.compileType(.{ .@"struct" = t }, .{});
    try self.write("){\n");

    var members = struct_inst.members.iterator();
    while (members.next()) |m| {
        try self.print(".{s} = ", .{m.key_ptr.*});
        try compile(self, m.value_ptr, .{
            .is_variable_declaration = true,
            .expected_type = t.members.get(m.key_ptr.*).?,
        });
        try self.write(",\n");
    }

    try self.write("}");
}

fn unionInstantiation(self: *Self, struct_inst: ast.Expression.StructInstantiation, u: Type.Union) !void {
    try self.write("(");
    try self.compileType(.{ .@"union" = u }, .{});
    try self.write("){ ");

    if (struct_inst.members.count() != 1) return utils.printErr(
        error.IllegalExpression,
        "comperr: Instantiation of union type '{s}' is missing a tag initialization ({f}).\n",
        .{ u.name, struct_inst.pos },
        .red,
    );

    // get used tag
    var it = struct_inst.members.iterator();
    const tag_and_payload = it.next().?;
    const tag_name = tag_and_payload.key_ptr.*;
    const payload_val = tag_and_payload.value_ptr.*;

    const m = try u.getMember(tag_name);

    try self.print(".tag = __zag_{s}_{s}, ", .{ u.tag_type.?.@"enum".inner_name, tag_name });

    try self.print(".payload = {{ .{s} = ", .{m.member_name});
    try compile(self, &payload_val, .{
        .expected_type = m.member_type,
        .is_variable_declaration = true,
    });
    try self.write(" } }");
}

fn member(self: *Self, expr: ast.Expression.Member) CompilerError!void {
    const parent_type: Type = try .infer(self, expr.parent.*);
    var deref_level: usize = 0;
    b: switch (parent_type) {
        .type => |t| continue :b t.*,
        .type_type => return errors.illegalMemberExpression(parent_type, expr.pos),

        .@"struct" => |@"struct"| if (@"struct".getProperty(expr.member_name)) |property| switch (property) {
            .variable => |variable| {
                if (!std.mem.eql(u8, self.module.name, @"struct".module.name) and !variable.is_pub)
                    return errors.badAccess(@"struct".name, expr.member_name, expr.pos);
                try self.write(variable.inner_name);
            },
            .member => {
                try self.write("(");
                for (0..deref_level) |_| try self.write("*");
                try compile(self, expr.parent, .{});
                try self.print(").{s}", .{expr.member_name});
            },
            .method => |method| try self.write(method.inner_name),
            .subtype => |subtype| try self.write(subtype.inner_name),
        } else return errors.undeclaredProperty(.{ .@"struct" = @"struct" }, expr.member_name, expr.pos),

        .@"enum" => |@"enum"| if (@"enum".getProperty(expr.member_name)) |property| switch (property) {
            .variable => |variable| {
                if (!std.mem.eql(u8, self.module.name, @"enum".module.name) and !variable.is_pub)
                    return errors.badAccess(@"enum".name, expr.member_name, expr.pos);
                try self.write(variable.inner_name);
            },
            .member => {
                // Check if this is a union tag enum (used internally) vs a regular user enum
                // Union tag enums have names ending in "_tag_type" and use the __zag_ prefix
                if (std.mem.endsWith(u8, @"enum".inner_name, "_tag_type")) {
                    // This is a union's internal tag enum
                    try self.print("__zag_{s}_{s}", .{ @"enum".inner_name, expr.member_name });
                } else {
                    // This is a regular user-defined enum
                    try self.print("{s}_{s}", .{ @"enum".inner_name, expr.member_name });
                }
            },
            .method => |method| try self.write(method.inner_name),
            .subtype => |subtype| try self.write(subtype.inner_name),
        } else return errors.undeclaredProperty(.{ .@"enum" = @"enum" }, expr.member_name, expr.pos),

        .@"union" => |@"union"| if (@"union".getProperty(expr.member_name)) |property| switch (property) {
            .variable => |variable| {
                if (!std.mem.eql(u8, self.module.name, @"union".module.name) and !variable.is_pub)
                    return errors.badAccess(@"union".name, expr.member_name, expr.pos);
                try self.write(variable.inner_name);
            },
            .member => {
                try self.write("(");
                for (0..deref_level) |_| try self.write("*");
                try compile(self, expr.parent, .{});
                try self.print(").payload.{s}", .{expr.member_name});
            },
            .method => |method| try self.write(method.inner_name),
            .subtype => |subtype| try self.write(subtype.inner_name),
        } else return errors.undeclaredProperty(.{ .@"union" = @"union" }, expr.member_name, expr.pos),

        .reference => |reference| {
            deref_level += 1;
            continue :b reference.inner.*;
        },

        .module => |module| if (module.symbols.get(expr.member_name)) |symbol| {
            if (!symbol.is_pub) return errors.badAccess(module.name, expr.member_name, expr.pos);
            try self.write(symbol.inner_name);
        } else return utils.printErr(
            error.UndeclaredProperty,
            "comperr: Module '{s}' has no member '{s}' ({f}).\n",
            .{ module.name, expr.member_name, expr.pos },
            .red,
        ),

        .slice => if (std.mem.eql(u8, expr.member_name, "len") or std.mem.eql(u8, expr.member_name, "ptr")) {
            try self.write("(");
            try compile(self, expr.parent, .{});
            try self.print(").{s}", .{expr.member_name});
        } else return utils.printErr(
            error.UndeclaredProperty,
            "comperr: Slice type only has members `ptr` and `len`, attempted to use member '{s}' ({f}).\n",
            .{ expr.member_name, expr.pos },
            .red,
        ),

        else => |other| return errors.illegalMemberExpression(other, expr.pos),
    }
}

fn assignment(self: *Self, expr: ast.Expression.Assignment) CompilerError!void {
    const expected_type: Type = try Type.infer(self, expr.assignee.*);
    const received_type: Type = try Type.infer(self, expr.value.*);

    if (!received_type.check(expected_type))
        return errors.typeMismatch(expected_type, received_type, expr.value.getPosition());

    if (!try self.getExpressionMutability(expr.assignee.*))
        return errors.badMutability(expr.pos);

    switch (expr.op) {
        .@"^=" => {
            try compile(self, expr.assignee, .{ .is_assignment = true });
            try self.write(" = pow(");
            try compile(self, expr.assignee, .{});
            try self.write(",");
            try compile(self, expr.value, .{ .expected_type = expected_type });
            try self.write(")");
        },
        else => {
            try compile(self, expr.assignee, .{ .is_assignment = true });
            try self.print(" {s} ", .{@tagName(expr.op)});
            try compile(self, expr.value, .{ .expected_type = expected_type });
        },
    }
}

fn binary(self: *Self, expr: ast.Expression.Binary) CompilerError!void {
    const lhs_t: Type = try .infer(self, expr.lhs.*);
    const rhs_t: Type = try .infer(self, expr.rhs.*);
    const expr_is_optional = (lhs_t == .optional and rhs_t == .@"typeof(nil)") or
        (lhs_t == .optional and rhs_t == .optional and lhs_t.optional.eql(rhs_t.optional.*)) or
        (rhs_t == .optional and lhs_t == .@"typeof(nil)");
    const expr_is_type = lhs_t == .type and rhs_t == .type;

    if (!((lhs_t.isNumeric() and rhs_t.isNumeric()) or
        (lhs_t == .bool and rhs_t == .bool) or
        (lhs_t == .reference and rhs_t == .reference) or
        (lhs_t == .@"enum" and rhs_t == .@"enum" and lhs_t.eql(rhs_t)) or
        expr_is_type or expr_is_optional))
        return utils.printErr(
            error.IllegalExpression,
            "comperr: Binary expression between non-numeric types is illegal. Received '{f}' {s} '{f}' ({f}).\n",
            .{ lhs_t, @tagName(expr.op), rhs_t, expr.pos },
            .red,
        );

    switch (expr.op) {
        .@"^" => if (self.solveComptimeExpression(.{ .binary = expr })) |comptime_expr| {
            try self.print("{f}", .{comptime_expr});
        } else |_| {
            try self.write("pow(");
            try compile(self, expr.lhs, .{});
            try self.write(", ");
            try compile(self, expr.rhs, .{});
            try self.write(")");
        },
        else => if (expr_is_optional) {
            self.currentSection().pos = self.currentSection().current_statement;

            try self.compileType(lhs_t, .{});
            const lhs_temp_name = try std.fmt.allocPrint(self.alloc, "_{x}", .{std.hash.Wyhash.hash(0, std.mem.asBytes(expr.lhs))});
            try self.print(" {s} = ", .{lhs_temp_name});
            try compile(self, expr.lhs, .{ .expected_type = lhs_t, .is_variable_declaration = true });
            try self.write(";\n");

            try self.compileType(if (rhs_t == .@"typeof(nil)") lhs_t else rhs_t, .{});
            const rhs_temp_name = try std.fmt.allocPrint(self.alloc, "_{x}", .{std.hash.Wyhash.hash(0, std.mem.asBytes(expr.rhs))});
            try self.print(" {s} = ", .{rhs_temp_name});
            try compile(self, expr.rhs, .{
                .expected_type = if (rhs_t == .@"typeof(nil)") lhs_t else rhs_t,
                .is_variable_declaration = true,
            });
            try self.write(";\n");

            self.currentSection().pos = self.currentWriter().items.len;

            try self.write("((");
            if (lhs_t == .@"typeof(nil)") {
                try self.write("false");
            } else {
                try self.write("(");
                try compile(self, expr.lhs, .{});
                try self.write(")");
                try self.write(".is_some");
            }

            try self.write(" == ");

            if (rhs_t == .@"typeof(nil)") {
                try self.write("false");
            } else {
                try self.write("(");
                try compile(self, expr.rhs, .{});
                try self.write(")");
                try self.write(".is_some");
            }

            try self.print(
                ") && (({[lhs]s}.is_some && {[rhs]s}.is_some) ? ({[lhs]s}.payload == {[rhs]s}.payload) : true))",
                .{ .lhs = lhs_temp_name, .rhs = rhs_temp_name },
            );
        } else if (expr_is_type) {
            try self.print("{}", .{lhs_t.type.eql(rhs_t.type.*)});
        } else {
            try self.write("(");
            try compile(self, expr.lhs, .{});
            try self.print(" {s} ", .{switch (expr.op) {
                .@"and", .but => "&&",
                .@"or" => "||",
                else => |op| @tagName(op),
            }});
            try compile(self, expr.rhs, .{});
            try self.write(")");
        },
    }
}

fn comparison(self: *Self, comp: ast.Expression.Comparison) CompilerError!void {
    if (comp.comparisons.len == 0) return;

    try self.write("(");

    var variables: std.ArrayList(u64) = try .initCapacity(self.alloc, comp.comparisons.len);
    defer variables.deinit(self.alloc);
    for (comp.comparisons) |i| variables.appendAssumeCapacity(std.hash.Wyhash.hash(0, std.mem.asBytes(i.right)));

    self.currentSection().pos = self.currentSection().current_statement;

    for (variables.items, 0..) |v, i| {
        const expr = comp.comparisons[i].right;
        const t: Type = try .infer(self, expr.*);
        try self.compileType(t, .{});
        try self.print(" _{} = ", .{v});
        try compile(self, expr, .{});
        try self.write(";\n");
    }

    self.currentSection().pos = self.currentWriter().items.len;

    const lhs: Type = try .infer(self, comp.left.*);
    if (!lhs.isNumeric() and lhs != .bool) return utils.printErr(
        error.IllegalExpression,
        "comperr: Comparison expression between non-numeric types is illegal. Received '{f}' ({f}).\n",
        .{ lhs, comp.left.getPosition() },
        .red,
    );
    try compile(self, comp.left, .{});

    for (comp.comparisons, 0..) |item, i| {
        const rhs_t: Type = try .infer(self, item.right.*);
        if (!rhs_t.isNumeric() and rhs_t != .bool) return utils.printErr(
            error.IllegalExpression,
            "comperr: Comparison expression between non-numeric types is illegal. Received '{f}' ({f}).\n",
            .{ rhs_t, item.right.getPosition() },
            .red,
        );

        if (i > 0) try self.print("_{} ", .{variables.items[i - 1]});
        try self.print(" {s} _{}", .{ @tagName(item.op), variables.items[i] });
        if (i < comp.comparisons.len - 1) try self.write(" &&");
    }

    try self.write(")");
}

fn call(self: *Self, call_expr: ast.Expression.Call) CompilerError!void {
    switch (call_expr.callee.*) {
        .member => |m| {
            if (try tryResolveStaticType(self, m.parent.*)) |static_type| switch (static_type) {
                inline .@"struct", .@"enum", .@"union" => |s, t| if (s.getProperty(m.member_name)) |property| switch (property) {
                    .variable => |v| return errors.expressionNotCallable(v.type, call_expr.callee.getPosition()),
                    .member => |member_type| return errors.expressionNotCallable(if (t == .@"enum") .usize else member_type, call_expr.callee.getPosition()),
                    .method => |method| {
                        try functionCall(self, Type.Function.fromMethod(
                            std.meta.stringToEnum(utils.CompoundTypeTag, @tagName(t)).?,
                            method,
                            s.module,
                        ).function, call_expr);
                        return;
                    },
                    .subtype => |st| return errors.expressionNotCallable(st.toType(), call_expr.callee.getPosition()),
                } else return errors.undeclaredProperty(static_type, m.member_name, call_expr.pos),
                .module => |module| if (module.symbols.get(m.member_name)) |symbol| {
                    if (!symbol.is_pub) return errors.badAccess(module.name, m.member_name, m.pos);
                    switch (symbol.type) {
                        .function => |function| {
                            try functionCall(self, function, call_expr);
                            return;
                        },
                        else => return errors.expressionNotCallable(symbol.type, call_expr.callee.getPosition()),
                    }
                } else return utils.printErr(
                    error.UndeclaredProperty,
                    "comperr: Module '{s}' has no member '{s}' ({f}).\n",
                    .{ module.name, m.member_name, call_expr.pos },
                    .red,
                ),
                else => {},
            };

            var parent_reference_level: i32 = 0;
            var reference_is_mut = self.getExpressionMutability(m.parent.*);

            // Need to resolve method to Type.Function for methodCall
            const parent = try Type.infer(self, m.parent.*);
            const method_func: Type.Function = b: switch (parent) {
                .@"struct" => |s| if (s.getProperty(m.member_name)) |prop| switch (prop) {
                    .method => |method| .{
                        .name = method.inner_name,
                        .inner_name = method.inner_name,
                        .params = method.params,
                        .generic_params = method.generic_params,
                        .return_type = method.return_type,
                        .definition = method.definition,
                        .module = s.module,
                    },
                    else => return errors.undeclaredProperty(parent, m.member_name, call_expr.pos),
                } else return errors.undeclaredProperty(parent, m.member_name, call_expr.pos),
                .@"union" => |u| if (u.getProperty(m.member_name)) |prop| switch (prop) {
                    .method => |method| .{
                        .name = method.inner_name,
                        .inner_name = method.inner_name,
                        .params = method.params,
                        .generic_params = method.generic_params,
                        .return_type = method.return_type,
                        .definition = method.definition,
                        .module = u.module,
                    },
                    else => return errors.undeclaredProperty(parent, m.member_name, call_expr.pos),
                } else return errors.undeclaredProperty(parent, m.member_name, call_expr.pos),
                .reference => |reference| {
                    if (reference.is_mut) reference_is_mut = true;

                    parent_reference_level += 1;
                    continue :b reference.inner.*;
                },
                .type => |t| continue :b t.*,
                .type_type => return errors.illegalMemberExpression(.type_type, m.pos),
                else => return errors.undeclaredProperty(parent, m.member_name, call_expr.pos),
            };
            try methodCall(self, call_expr, m, method_func);
        },
        .generic => |g| switch (g.lhs.*) {
            .member => |m| {
                const func_type = try Type.infer(self, call_expr.callee.*);
                switch (func_type) {
                    .function => |f| {
                        if (try tryResolveStaticType(self, m.parent.*)) |_|
                            try functionCall(self, f, call_expr)
                        else
                            try methodCall(self, call_expr, m, f);
                        return;
                    },
                    else => {},
                }
            },
            else => switch (try Type.infer(self, call_expr.callee.*)) {
                .function => |function| try functionCall(self, function, call_expr),
                else => |other| return errors.expressionNotCallable(other, call_expr.callee.getPosition()),
            },
        },
        else => switch (try Type.infer(self, call_expr.callee.*)) {
            .function => |function| try functionCall(self, function, call_expr),
            else => |other| return errors.expressionNotCallable(other, call_expr.callee.getPosition()),
        },
    }
}

fn tryResolveStaticType(self: *Self, expr: ast.Expression) !?Type {
    switch (expr) {
        .ident => |ident| {
            const item = self.getScopeItem(ident.payload) catch return null;
            return switch (item) {
                .type => |t| t.type,
                .module => |m| .{ .module = m },
                else => null,
            };
        },
        .member => |m| {
            const parent = (try tryResolveStaticType(self, m.parent.*)) orelse return null;
            switch (parent) {
                .module => |module| {
                    if (module.symbols.get(m.member_name)) |sym| {
                        switch (sym.type) {
                            .@"struct", .@"enum", .@"union" => return sym.type,
                            .module => return .{ .module = sym.type.module },
                            else => return null,
                        }
                    }
                    return null;
                },
                else => return null,
            }
        },
        else => return null,
    }
}

fn methodCall(self: *Self, call_expr: ast.Expression.Call, m: ast.Expression.Member, method: Type.Function) !void {
    var parent_reference_level: i32 = 0;
    var reference_is_mut = try self.getExpressionMutability(m.parent.*);

    var is_instance_method = false;
    const parent: Type = try .infer(self, m.parent.*);
    b: switch (parent) {
        inline .@"struct", .@"union" => |@"struct"| {
            if (!is_instance_method) {
                if (method.params.items.len == 0)
                    return errors.notAnInstanceMethod(@"struct".name, m.member_name, m.pos);

                const first_type = method.params.items[0].type;
                if (!first_type.check(parent) and
                    (first_type != .reference or !first_type.reference.inner.check(parent)) and
                    (parent != .reference or !parent.reference.inner.check(first_type)))
                    return errors.notAnInstanceMethod(@"struct".name, m.member_name, m.pos);
            }

            const expected_args = if (is_instance_method) method.params.items.len else method.params.items.len - 1;
            const received_args = call_expr.args.len;
            if (expected_args != received_args) return errors.argumentCountMismatch(
                expected_args,
                received_args,
                call_expr.pos,
            );

            try self.print("{s}(", .{method.name});
            if (method.params.items.len > 0) {
                const expected_type = method.params.items[0].type;
                switch (expected_type) {
                    .reference => |param_arg| if (param_arg.is_mut and !reference_is_mut) return utils.printErr(
                        error.BadMutability,
                        "comperr: '{s}.{s}' method requires &mut {s}, but &{s} was passed ({f}).\n",
                        .{ @"struct".name, m.member_name, @"struct".name, @"struct".name, m.pos },
                        .red,
                    ),
                    .@"struct" => if (!(parent.check(expected_type) or parent == .reference and
                        parent.reference.inner.check(expected_type))) unreachable,
                    else => {},
                }

                for (method.params.items[1..], 0..) |expected_param_type, i| {
                    const received_expr = call_expr.args[i];
                    const received_type: Type = try .infer(self, received_expr);
                    if (expected_param_type.type != .variadic and !expected_param_type.type.eql(received_type)) return errors.typeMismatch(
                        expected_param_type.type,
                        received_type,
                        received_expr.getPosition(),
                    );
                }

                const ref_level_diff = parent_reference_level - b2: {
                    var self_method_reference_level: i32 = 0;
                    count_ref_level: switch (method.params.items[0].type) {
                        .reference => |reference| {
                            self_method_reference_level += 1;
                            continue :count_ref_level reference.inner.*;
                        },
                        else => break :count_ref_level,
                    }
                    break :b2 self_method_reference_level;
                };

                for (0..@abs(ref_level_diff)) |_| try self.write(
                    if (ref_level_diff > 0) "*" else if (ref_level_diff < 0) "&" else unreachable,
                );
                try compile(self, m.parent, .{});
                if (method.params.items.len > 1) try self.write(", ");
                for (call_expr.args, 1..) |*arg, i| {
                    try compile(self, arg, .{});
                    if (i < call_expr.args.len) try self.write(", ");
                }
            }

            try self.write(")");
        },
        .reference => |reference| {
            if (reference.is_mut) reference_is_mut = true;

            parent_reference_level += 1;
            continue :b reference.inner.*;
        },
        .type => |t| {
            is_instance_method = true;
            continue :b t.*;
        },
        .type_type => return errors.illegalMemberExpression(.type_type, m.pos),
        else => |other| return errors.illegalMemberExpression(other, m.pos),
    }
}

fn functionCall(self: *Self, function: Type.Function, call_expr: ast.Expression.Call) !void {
    if (function.generic_instantiation) |inst| {
        if (std.mem.eql(u8, inst.base_name, "sizeof")) {
            if (call_expr.args.len != 0) return errors.argumentCountMismatch(
                0,
                call_expr.args.len,
                call_expr.pos,
            );

            try self.write("sizeof(");
            try self.compileType(inst.args[0].type, .{});
            try self.write(")");
            return;
        } else if (std.mem.eql(u8, inst.base_name, "cast")) {
            if (call_expr.args.len != 1) return errors.argumentCountMismatch(
                1,
                call_expr.args.len,
                call_expr.pos,
            );

            const source_type = try Type.infer(self, call_expr.args[0]);
            const target_type = inst.args[0].type;

            switch (source_type) {
                .slice => switch (target_type) {
                    .reference => {
                        // cast from slice to pointer: cast the ptr field
                        try self.write("((");
                        try self.compileType(target_type, .{});
                        try self.write(")((");
                        try compile(self, &call_expr.args[0], .{});
                        try self.write(").ptr))");
                        return;
                    },
                    .slice => {
                        // cast from slice to slice: cast ptr field and keep len
                        try self.write("((");
                        try self.compileType(target_type, .{});
                        try self.write("){ .ptr = (void*)((");
                        try compile(self, &call_expr.args[0], .{});
                        try self.write(").ptr), .len = (");
                        try compile(self, &call_expr.args[0], .{});
                        try self.write(").len })");
                        return;
                    },
                    else => {},
                },
                else => {},
            }

            try self.write("(");
            try self.compileType(inst.args[0].type, .{});
            try self.write(")");
            try compile(self, &call_expr.args[0], .{});
            return;
        } else if (std.mem.eql(u8, inst.base_name, "xor")) {
            if (call_expr.args.len != 2) return errors.argumentCountMismatch(
                2,
                call_expr.args.len,
                call_expr.pos,
            );

            try compile(self, &call_expr.args[0], .{});
            try self.write(" ^ ");
            try compile(self, &call_expr.args[1], .{});
            return;
        }

        // For other generic function instantiations, ensure they're in pending list
        if (function.definition) |def| {
            // Check if already pending
            var already_pending = false;
            for (self.pending_instantiations.items) |pending| {
                if (pending.t == .function and std.mem.eql(u8, pending.inner_name, function.name)) {
                    already_pending = true;
                    break;
                }
            }

            if (!already_pending)
                try self.pending_instantiations.append(self.alloc, .{
                    .inner_name = function.name,
                    .args = inst.args,
                    .module = function.module,
                    .t = .{ .function = def.* },
                });
        }
    }

    const variadic_arg: ?usize = b: {
        for (function.params.items, 0..) |param_type, i|
            if (param_type.type == .variadic) break :b i;
        break :b null;
    };

    const expected_args = if (variadic_arg) |_| function.params.items.len - 1 else function.params.items.len;
    const received_args = call_expr.args.len;

    if (variadic_arg) |_| {
        if (received_args < expected_args) return errors.argumentCountMismatch(
            expected_args - 1,
            received_args,
            call_expr.pos,
        );
    } else if (expected_args != received_args) return errors.argumentCountMismatch(
        expected_args,
        received_args,
        call_expr.pos,
    );

    for (function.params.items[0 .. variadic_arg orelse function.params.items.len], 0..) |expected_param, i| {
        const expected_type = expected_param.type;
        const received_expr = call_expr.args[i];
        const received_type = try Type.infer(self, received_expr);

        if (expected_type != .variadic and !received_type.check(expected_type))
            return errors.typeMismatch(expected_type, received_type, received_expr.getPosition());

        switch (expected_type) {
            .slice => |exp_slice| switch (received_type) {
                .slice => |rec_slice| if (exp_slice.is_mut and !rec_slice.is_mut)
                    return errors.typeMismatch(expected_type, received_type, received_expr.getPosition()),
                else => {},
            },
            else => {},
        }
    }

    try compile(self, call_expr.callee, .{});

    try self.write("(");
    for (call_expr.args, 1..) |*e, i| {
        const expected_type = function.params.items[i - 1].type;
        try compile(self, e, .{ .expected_type = if (expected_type == .variadic) null else expected_type });
        if (i < call_expr.args.len) try self.write(", ");
    }
    try self.write(")");
}
