const std = @import("std");

const utils = @import("utils");

const ast = @import("Parser").ast;
const hash = @import("Parser").hash;

const Self = @import("Compiler.zig");
const Type = @import("Type.zig").Type;

const CompilerError = Self.CompilerError;

pub fn compile(
    self: *Self,
    expression: *const ast.Expression,
    opts: struct {
        is_const: bool = false,
        expected_type: ?Type = null, // null means infer the type from the expression
    },
) CompilerError!void {
    if (opts.expected_type) |expected_type| {
        const received_type: Type = try .infer(self, expression.*);

        if (!expected_type.eql(received_type) and !received_type.convertsTo(expected_type))
            return utils.printErr(
                error.TypeMismatch,
                "comperr: Expected '{f}', received '{f}' ({f}).\n",
                .{ expected_type, received_type, expression.getPosition() },
                .red,
            );

        if (!expected_type.eql(received_type) and expected_type == .optional) switch (received_type) {
            .@"typeof(null)" => try self.print(
                "({s}){{ .is_some = false }}",
                .{self.zag_header_contents.get(expected_type) orelse unreachable},
            ),
            else => if (expected_type.optional.convertsTo(received_type)) {
                try self.print(
                    "({s}){{ .is_some = true, .payload = ",
                    .{self.zag_header_contents.get(expected_type) orelse unreachable},
                );
                try compile(self, expression, .{ .expected_type = expected_type.optional.* });
                try self.write(" }");
            } else return utils.printErr(
                error.TypeMismatch,
                "comperr: Expected '{f}', received '{f}' ({f}).\n",
                .{ expected_type, received_type, expression.getPosition() },
                .red,
            ),
        } else try compile(self, expression, .{ .is_const = opts.is_const });
    } else switch (expression.*) {
        .assignment => |a| try assignment(self, a),
        .block => |block| try self.compileBlock(block.block, .{}),
        .binary => |b| try binary(self, b),
        .float => |float| try self.print("{}", .{float.float}),
        .int => |int| try self.print("{}", .{int.int}),
        .uint => |uint| try self.print("{}", .{uint.uint}),
        .string => |string| try self.print("\"{s}\"", .{string.string}),
        .char => |char| try self.print("'{c}'", .{char.char}),
        .call => |c| try call(self, c),
        .member => |m| try member(self, m),
        .range => std.debug.print("illegal range expression\n", .{}),
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
        .struct_instantiation => |struct_inst| {
            try self.print("({s}){{\n", .{struct_inst.name});
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
            if (opts.is_const) try self.write("const ");
            try self.compileType(try .fromAst(self, array.type));
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
        .bad_node => unreachable,
    }
}

fn member(
    self: *Self,
    expr: ast.Expression.Member,
) CompilerError!void {
    const parent_type: Type = try .infer(self, expr.parent.*);
    var delimiter: enum { @".", @"->" } = .@".";
    b: switch (parent_type) {
        .@"struct" => |@"struct"| {
            if (@"struct".getProperty(expr.member_name)) |property| switch (property) {
                .member => {
                    try compile(self, expr.parent, .{});
                    try self.print("{s}{s}", .{
                        @tagName(delimiter),
                        expr.member_name,
                    });
                    delimiter = .@".";
                },
                .method => |method| try self.print("&{s}", .{method.inner_name}),
            } else return utils.printErr(
                error.UndeclaredProperty,
                "comperr: '{f}' has no member '{s}' ({f})\n",
                .{ parent_type, expr.member_name, expr.pos },
                .red,
            );
        },
        .reference => |reference| {
            delimiter = .@"->";
            continue :b reference.inner.*;
        },
        else => return utils.printErr(
            error.IllegalExpression,
            "comperr: Member expression on '{f}' is illegal\n",
            .{parent_type},
            .red,
        ),
    }
}

fn assignment(
    self: *Self,
    expr: ast.Expression.Assignment,
) CompilerError!void {
    const expected_type: Type = try .infer(self, expr.assignee.*);
    const received_type: Type = try .infer(self, expr.value.*);
    if (!expected_type.eql(received_type) and
        !received_type.convertsTo(expected_type))
        return utils.printErr(
            error.TypeMismatch,
            "comperr: Type of expression doesn't match explicit type. Expected: '{f}', received '{f}' ({f}).\n",
            .{ expected_type, received_type, expr.value.getPosition() },
            .red,
        );

    try compile(self, expr.assignee, .{});
    try self.print(" {s} ", .{@tagName(expr.op)});
    try compile(self, expr.value, .{ .expected_type = expected_type });
}

fn binary(
    self: *Self,
    expr: ast.Expression.Binary,
) CompilerError!void {
    try compile(self, expr.lhs, .{});
    try self.print(" {s} ", .{@tagName(expr.op)});
    try compile(self, expr.rhs, .{});
}

fn call(
    self: *Self,
    call_expr: ast.Expression.Call,
) CompilerError!void {
    switch (call_expr.callee.*) {
        .member => |m| {
            var parent_reference_level: i32 = 0;
            var reference_is_mut = switch (m.parent.*) {
                .ident => |ident| try self.getSymbolMutability(ident.ident),
                else => false,
            };

            const parent: Type = try .infer(self, m.parent.*);
            b: switch (parent) {
                .@"struct" => |@"struct"| if (@"struct".methods.get(m.member_name)) |method| {
                    const param_type = method.params.items[0].*;
                    switch (param_type) {
                        .reference => |param_arg| if (param_arg.is_mut and !reference_is_mut) return utils.printErr(
                            error.BadMutability,
                            "comperr: '{s}.{s}' method requires &mut {s}, but &{s} was passed ({f}).\n",
                            .{ @"struct".name, m.member_name, @"struct".name, @"struct".name, m.pos },
                            .red,
                        ),
                        .@"struct" => if (!param_type.eql(parent)) unreachable,
                        else => unreachable,
                    }

                    const expected_args = method.params.items.len - 1;
                    const received_args = call_expr.args.items.len;
                    if (expected_args < received_args) return utils.printErr(
                        error.TooManyArguments,
                        "comperr: Too many arguments in method call at {f}. Expected {}, found {}\n",
                        .{
                            call_expr.args.items[0].getPosition(),
                            expected_args,
                            received_args,
                        },
                        .red,
                    ) else if (expected_args > received_args) return utils.printErr(
                        error.MissingArguments,
                        "comperr: Missing arguments in method call at {f}. Expected {}, found {}\n",
                        .{
                            call_expr.args.items[0].getPosition(),
                            expected_args,
                            received_args,
                        },
                        .red,
                    );

                    for (method.params.items[1..], 0..) |param, i| {
                        const received_expr = call_expr.args.items[i];
                        const received_type: Type = try .infer(self, received_expr);
                        if (!param.eql(received_type)) return utils.printErr(
                            error.TypeMismatch,
                            "comperr: type doesn't match method signature at {f}. Expected '{f}', got '{f}'\n",
                            .{ received_expr.getPosition(), param, received_type },
                            .red,
                        );
                    }

                    const ref_level_diff = parent_reference_level - b2: {
                        var self_method_reference_level: i32 = 0;
                        count_ref_level: switch (method.params.items[0].*) {
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
                    try self.write(", ");
                    for (call_expr.args.items, 1..) |*arg, i| {
                        try compile(self, arg, .{});
                        if (i < call_expr.args.items.len) try self.write(", ");
                    }
                    try self.write(")");
                } else return utils.printErr(
                    error.MemberIsNotAMethod,
                    "comperr: {s}.{s} is not a method\n",
                    .{ @"struct".name, m.member_name },
                    .red,
                ),
                .reference => |reference| {
                    if (reference.is_mut) reference_is_mut = true;

                    parent_reference_level += 1;
                    continue :b reference.inner.*;
                },
                else => |other| return utils.printErr(
                    error.IllegalExpression,
                    "comperr: member expression on {s} is illegal\n",
                    .{@tagName(other)},
                    .red,
                ),
            }
        },
        else => switch (try Type.infer(self, call_expr.callee.*)) {
            .function => |function| {
                for (function.params.items, 0..) |param, i| {
                    const received_expr = call_expr.args.items[i];
                    const received_type: Type = try .infer(self, received_expr);
                    if (!param.eql(try .infer(self, call_expr.args.items[i]))) {
                        return utils.printErr(
                            error.TypeMismatch,
                            "comperr: Type doesn't match function signature at {f}. Expected '{f}', got '{f}' ({f}).\n",
                            .{ received_expr.getPosition(), param, received_type, call_expr.pos },
                            .red,
                        );
                    }
                }

                try compile(self, call_expr.callee, .{});
                try self.write("(");
                for (call_expr.args.items, 1..) |*e, i| {
                    try compile(self, e, .{});
                    if (i < call_expr.args.items.len) try self.write(", ");
                }
                try self.write(")");
            },
            else => |other| return utils.printErr(
                error.IllegalExpression,
                "comperr: Expression of type '{f}' is not callable ({f})\n",
                .{ other, call_expr.callee.getPosition() },
                .red,
            ),
        },
    }
}
