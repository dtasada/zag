const std = @import("std");

const utils = @import("utils");

const ast = @import("Parser").ast;
const errors = @import("errors.zig");
const statements = @import("statements.zig");
const hash = @import("Parser").hash;

const Self = @import("Compiler.zig");
const Type = @import("Type.zig").Type;

const CompilerError = Self.CompilerError;

pub fn compile(
    self: *Self,
    expression: *const ast.Expression,
    opts: struct {
        binding_mut: bool = false,
        expected_type: ?Type = null, // null means infer the type from the expression
    },
) CompilerError!void {
    if (opts.expected_type) |expected_type| {
        const received_type: Type = try .infer(self, expression.*);

        if (!self.checkType(expected_type, received_type))
            return errors.typeMismatch(expected_type, received_type, expression.getPosition());

        if (!expected_type.eql(received_type)) switch (expected_type) {
            .optional => |opt| switch (received_type) {
                .@"typeof(null)" => try self.print(
                    "({s}){{ .is_some = false }}",
                    .{self.zag_header_contents.get(expected_type) orelse unreachable},
                ),
                else => if (opt.convertsTo(received_type)) {
                    try self.print(
                        "({s}){{ .is_some = true, .payload = ",
                        .{self.zag_header_contents.get(expected_type) orelse unreachable},
                    );
                    try compile(self, expression, .{ .expected_type = opt.* });
                    try self.write(" }");
                } else return errors.typeMismatch(
                    expected_type,
                    received_type,
                    expression.getPosition(),
                ),
            },
            else => try compile(self, expression, .{ .binding_mut = opts.binding_mut }),
        } else try compile(self, expression, .{ .binding_mut = opts.binding_mut });
    } else switch (expression.*) {
        .assignment => |a| try assignment(self, a),
        .block => |block| try self.compileBlock(block.block, .{}),
        .binary => |b| try binary(self, b),
        .comparison => |comp| try comparison(self, comp),
        .float => |float| try self.print("{}", .{float.float}),
        .int => |int| try self.print("{}", .{int.int}),
        .uint => |uint| try self.print("{}", .{uint.uint}),
        .string => |string| try self.print("\"{s}\"", .{string.string}),
        .char => |char| try self.print("'{c}'", .{char.char}),
        .call => |c| try call(self, c),
        .member => |m| try member(self, m),
        .range => |range| return utils.printErr(
            error.IllegalExpression,
            "comperr: Illegal range expression at {f}.\n",
            .{range.pos},
            .red,
        ),
        .prefix => |prefix| {
            try self.write(switch (prefix.op) {
                .@"-" => "-",
                .@"!" => "!",
            });
            try compile(self, prefix.rhs, .{});
        },
        .ident => |ident| if (self.getSymbolType(ident.ident) catch null) |_|
            try self.write(ident.ident)
        else
            return utils.printErr(
                error.UnknownSymbol,
                "comperr: Unknown symbol '{s}' at {f}.\n",
                .{ ident.ident, expression.getPosition() },
                .red,
            ),
        .struct_instantiation => |struct_inst| switch (try Type.infer(self, struct_inst.type_expr.*)) {
            .@"struct" => |s| try structInstantiation(self, struct_inst, s),
            .@"union" => |u| try unionInstantiation(self, struct_inst, u),
            else => unreachable,
        },
        .reference => |reference| {
            switch (reference.inner.*) {
                .ident => |ident| if (reference.is_mut and !try self.getSymbolMutability(ident.ident))
                    return utils.printErr(
                        error.BadMutability,
                        "comperr: Can't use a mutable reference to immutable binding '{s}' ({f}).\n",
                        .{ ident.ident, reference.pos },
                        .red,
                    ),
                else => {},
            }

            try self.write("&");
            try compile(self, reference.inner, .{});
        },
        .array_instantiation => |array| {
            try self.write("(");
            if (opts.binding_mut) try self.write("const ");
            try self.compileType(try .fromAst(self, array.type), .{});
            try self.print("[]){{", .{});
            for (array.contents.items, 1..) |*item, i| {
                try compile(self, item, .{});
                if (i < array.contents.items.len) try self.write(", ");
            }
            try self.write("}");
        },
        .index => |index| {
            try compile(self, index.lhs, .{});
            try self.write("[");
            try compile(self, index.index, .{});
            try self.write("]");
        },
        .@"if" => |@"if"| {
            try self.write("((");
            try compile(self, @"if".condition, .{});
            try self.write(") ? ");

            if (@"if".capture != null)
                std.debug.print("unimplemented if expression capture\n", .{});

            try compile(self, @"if".body, .{});

            if (@"if".@"else") |@"else"| {
                try self.write(" : ");
                try compile(self, @"else", .{});
                try self.write(")");
            } else return utils.printErr(
                error.MissingElseClause,
                "comperr: If expression must contain an else clause ({f})\n",
                .{@"if".pos},
                .red,
            );
        },
        .generic => |_| @panic(""),
        .match => |m| try match(self, m),
        .bad_node => unreachable,
    }
}

fn match(self: *Self, m: ast.Expression.Match) !void {
    switch (try Type.infer(self, m.condition.*)) {
        .@"union" => |@"union"| {
            try self.write("switch ((");
            try compile(self, m.condition, .{});
            try self.write(").tag) {\n");
            self.indent_level += 1;

            for (m.cases.items) |case| {
                switch (case.condition) {
                    .opts => |cases| for (cases.items) |c| {
                        try self.indent();
                        try self.write("case ");
                        switch (c) {
                            .ident => |ident| {
                                const mem = @"union".getMember(ident.ident) catch return utils.printErr(
                                    error.IllegalExpression,
                                    "comperr: Union type '{s}' doesn't have member '{s}' ({f}).\n",
                                    .{ @"union".name, ident.ident, c.getPosition() },
                                    .red,
                                );
                                try self.print("__zag_{s}_tag_type_{s}", .{ @"union".name, mem.member_name });
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
                    .@"else" => {
                        try self.indent();
                        try self.write("default:\n");
                    },
                }
                self.indent_level += 1;

                try self.indent();
                std.debug.print("case.result: {}\n", .{case.result});
                try statements.compile(self, &case.result);
                try self.indent();
                try self.write(";break;\n");

                self.indent_level -= 1;
            }

            self.indent_level -= 1;
            try self.indent();
            try self.write("}");
        },
        else => @panic("unimplemented!"),
    }
}

fn structInstantiation(self: *Self, struct_inst: ast.Expression.StructInstantiation, t: Type.Struct) !void {
    try self.write("(");
    try self.compileType(.{ .@"struct" = t }, .{});
    try self.write("){\n");
    self.indent_level += 1;

    var members = struct_inst.members.iterator();
    while (members.next()) |m| {
        try self.indent();
        try self.print(".{s} = ", .{m.key_ptr.*});
        try compile(self, m.value_ptr, .{});
        try self.write(",\n");
    }

    self.indent_level -= 1;
    try self.indent();
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

    try self.print(".tag = __zag_{s}_tag_type_{s}, ", .{ u.name, tag_name });

    try self.print(".payload = {{ .{s} = ", .{m.member_name});
    try compile(self, &payload_val, .{ .expected_type = m.member_type.* });
    try self.write(" } }");
}

fn member(self: *Self, expr: ast.Expression.Member) CompilerError!void {
    const parent_type: Type = try .infer(self, expr.parent.*);
    var delimiter: enum { @".", @"->" } = .@".";
    b: switch (parent_type) {
        .@"struct" => |@"struct"| if (@"struct".getProperty(expr.member_name)) |property| switch (property) {
            .member => {
                try compile(self, expr.parent, .{});
                try self.print("{s}{s}", .{
                    @tagName(delimiter),
                    expr.member_name,
                });
                delimiter = .@".";
            },
            .method => |method| try self.write(method.inner_name),
        } else return errors.undeclaredProperty(parent_type, expr.member_name, expr.pos),

        .reference => |reference| {
            delimiter = .@"->";
            continue :b reference.inner.*;
        },

        .module => |module| if (module.symbols.get(expr.member_name)) |symbol| {
            try self.write(symbol.name);
        } else return utils.printErr(
            error.UndeclaredProperty,
            "comperr: Module '{s}' has no member '{s}' ({f}).\n",
            .{ module.name, expr.member_name, expr.pos },
            .red,
        ),

        .@"enum" => |@"enum"| if (@"enum".getProperty(expr.member_name)) |property| switch (property) {
            .member => try self.print("__zag_{s}_{s}", .{ @"enum".name, expr.member_name }),
            .method => |method| try self.write(method.inner_name),
        } else return errors.undeclaredProperty(parent_type, expr.member_name, expr.pos),

        .@"union" => |@"union"| if (@"union".getProperty(expr.member_name)) |property| switch (property) {
            .member => {
                try compile(self, expr.parent, .{});
                try self.print("{s}payload.{s}", .{
                    @tagName(delimiter),
                    expr.member_name,
                });
                delimiter = .@".";
            },
            .method => |method| try self.write(method.inner_name),
        } else return errors.undeclaredProperty(parent_type, expr.member_name, expr.pos),

        else => return utils.printErr(
            error.IllegalExpression,
            "comperr: Member expression on '{f}' is illegal ({f}).\n",
            .{ parent_type, expr.pos },
            .red,
        ),
    }
}

fn assignment(self: *Self, expr: ast.Expression.Assignment) CompilerError!void {
    const expected_type: Type = try .infer(self, expr.assignee.*);
    const received_type: Type = try .infer(self, expr.value.*);

    if (!self.checkType(expected_type, received_type))
        return errors.typeMismatch(expected_type, received_type, expr.value.getPosition());

    try compile(self, expr.assignee, .{});
    try self.print(" {s} ", .{@tagName(expr.op)});
    try compile(self, expr.value, .{ .expected_type = expected_type });
}

fn binary(self: *Self, expr: ast.Expression.Binary) CompilerError!void {
    try compile(self, expr.lhs, .{});
    try self.print(" {s} ", .{switch (expr.op) {
        .@"and", .but => "&&",
        .@"or" => "||",
        else => |op| @tagName(op),
    }});
    try compile(self, expr.rhs, .{});
}

fn comparison(self: *Self, comp: ast.Expression.Comparison) CompilerError!void {
    if (comp.comparisons.items.len == 0) return;

    try self.write("(");

    var prev_operand: *const ast.Expression = comp.left;

    for (comp.comparisons.items, 0..) |item, i| {
        if (i > 0) try self.write(" && ");

        try self.write("(");
        try compile(self, prev_operand, .{});
        try self.print(" {s} ", .{switch (item.op) {
            .@"==" => "==",
            .@"!=" => "!=",
            .@"<" => "<",
            .@">" => ">",
            .@"<=" => "<=",
            .@">=" => ">=",
            else => unreachable,
        }});
        try compile(self, item.right, .{});
        try self.write(")");

        prev_operand = item.right;
    }

    try self.write(")");
}

fn call(self: *Self, call_expr: ast.Expression.Call) CompilerError!void {
    switch (call_expr.callee.*) {
        .member => |m| {
            // Check if member access resolves to a static type (e.g. lib.Vector2.init)
            if (try tryResolveStaticType(self, m.parent.*)) |static_type| {
                switch (static_type) {
                    .@"struct" => |@"struct"| if (@"struct".getProperty(m.member_name)) |property| switch (property) {
                        .member => |member_type| return errors.expressionNotCallable(member_type.*, call_expr.callee.getPosition()),
                        .method => |method| {
                            try functionCall(self, .{
                                .params = method.params,
                                .return_type = method.return_type,
                            }, call_expr);
                            return;
                        },
                    } else return errors.undeclaredProperty(static_type, m.member_name, call_expr.pos),
                    .module => |module| if (module.symbols.get(m.member_name)) |symbol| switch (symbol.type) {
                        .function => |function| {
                            try functionCall(self, function, call_expr);
                            return;
                        },
                        else => return errors.expressionNotCallable(symbol.type, call_expr.callee.getPosition()),
                    } else return utils.printErr(
                        error.UndeclaredProperty,
                        "comperr: Module '{s}' has no member '{s}' ({f}).\n",
                        .{ module.name, m.member_name, call_expr.pos },
                        .red,
                    ),
                    else => {},
                }
            }
            try methodCall(self, call_expr, m);
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
            const item = self.getScopeItem(ident.ident) catch return null;
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

fn methodCall(self: *Self, call_expr: ast.Expression.Call, m: ast.Expression.Member) !void {
    var parent_reference_level: i32 = 0;
    var reference_is_mut = switch (m.parent.*) {
        .ident => |ident| try self.getSymbolMutability(ident.ident),
        else => false,
    };

    const parent: Type = try .infer(self, m.parent.*);
    b: switch (parent) {
        inline .@"struct", .@"union" => |@"struct"| if (@"struct".getProperty(m.member_name)) |property| switch (property) {
            .method => |method| {
                if (!parent.convertsTo(method.params.items[0].type))
                    return utils.printErr(
                        error.IllegalExpression,
                        "comperr: Illegal expression: '{s}.{s}' is not an instance method ({f}).\n",
                        .{ @"struct".name, m.member_name, m.pos },
                        .red,
                    );

                const expected_type = method.params.items[0].type;
                switch (expected_type) {
                    .reference => |param_arg| if (param_arg.is_mut and !reference_is_mut) return utils.printErr(
                        error.BadMutability,
                        "comperr: '{s}.{s}' method requires &mut {s}, but &{s} was passed ({f}).\n",
                        .{ @"struct".name, m.member_name, @"struct".name, @"struct".name, m.pos },
                        .red,
                    ),
                    .@"struct" => if (!expected_type.eql(parent)) unreachable,
                    else => {},
                }

                const expected_args = method.params.items.len - 1;
                const received_args = call_expr.args.items.len;
                if (expected_args < received_args) return errors.tooManyArguments(
                    expected_args,
                    received_args,
                    call_expr.args.items[0].getPosition(),
                ) else if (expected_args > received_args) return errors.missingArguments(
                    expected_args,
                    received_args,
                    call_expr.args.items[0].getPosition(),
                );

                for (method.params.items[1..], 0..) |expected_param_type, i| {
                    const received_expr = call_expr.args.items[i];
                    const received_type: Type = try .infer(self, received_expr);
                    if (expected_param_type.type != .variadic and !expected_param_type.type.eql(received_type)) return utils.printErr(
                        error.TypeMismatch,
                        "comperr: Type doesn't match method signature at {f}. Expected '{f}', got '{f}'\n",
                        .{ received_expr.getPosition(), expected_param_type.type, received_type },
                        .red,
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
                try self.print("{s}(", .{method.inner_name});
                for (0..@abs(ref_level_diff)) |_|
                    try self.write(
                        if (ref_level_diff > 0) "*" else if (ref_level_diff < 0)
                            "&"
                        else
                            unreachable,
                    );
                try compile(self, m.parent, .{});
                if (method.params.items.len > 1) try self.write(", ");
                for (call_expr.args.items, 1..) |*arg, i| {
                    try compile(self, arg, .{});
                    if (i < call_expr.args.items.len) try self.write(", ");
                }
                try self.write(")");
            },
            .member => return utils.printErr(
                error.MemberIsNotAMethod,
                "comperr: {s}.{s} is not a method\n",
                .{ @"struct".name, m.member_name },
                .red,
            ),
        } else return errors.undeclaredProperty(parent, m.member_name, call_expr.pos),
        .reference => |reference| {
            if (reference.is_mut) reference_is_mut = true;

            parent_reference_level += 1;
            continue :b reference.inner.*;
        },
        else => |other| return utils.printErr(
            error.IllegalExpression,
            "comperr: Member expression on {s} is illegal ({f}).\n",
            .{ @tagName(other), m.pos },
            .red,
        ),
    }
}

fn functionCall(self: *Self, function: Type.Function, call_expr: ast.Expression.Call) !void {
    const expected_args = function.params.items.len;
    const received_args = call_expr.args.items.len;

    const variadic_arg: ?usize = b: {
        for (function.params.items, 0..) |param_type, i|
            if (param_type.type == .variadic)
                break :b i;

        break :b null;
    };

    if (variadic_arg != null) {
        if (received_args < expected_args - 1) return errors.missingArguments(
            expected_args - 1,
            received_args,
            call_expr.args.items[0].getPosition(),
        );
    } else if (expected_args < received_args) return errors.tooManyArguments(
        expected_args,
        received_args,
        call_expr.args.items[0].getPosition(),
    ) else if (expected_args > received_args) return errors.missingArguments(
        expected_args,
        received_args,
        call_expr.args.items[0].getPosition(),
    );

    for (function.params.items[0 .. variadic_arg orelse function.params.items.len], 0..) |expected_type, i| {
        const received_expr = call_expr.args.items[i];
        const received_type: Type = try .infer(self, received_expr);
        if (expected_type.type != .variadic and
            !expected_type.type.eql(try .infer(self, call_expr.args.items[i])))
            return errors.typeMismatch(expected_type.type, received_type, received_expr.getPosition());
    }

    try compile(self, call_expr.callee, .{});
    try self.write("(");
    for (call_expr.args.items, 1..) |*e, i| {
        try compile(self, e, .{});
        if (i < call_expr.args.items.len) try self.write(", ");
    }
    try self.write(")");
}
