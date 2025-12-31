const std = @import("std");

const ast = @import("Parser").ast;

const Self = @import("Compiler.zig");
const Type = @import("Type.zig").Type;

const CompilerError = Self.CompilerError;

pub fn compile(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    expression: *const ast.Expression,
    opts: struct { is_const: bool = false },
) CompilerError!void {
    switch (expression.*) {
        .assignment => |a| try assignment(self, file_writer, a),
        .block => |block| try self.compileBlock(file_writer, block),
        .binary => |b| try binary(self, file_writer, b),
        .float => |float| try self.print(file_writer, "{}", .{float}),
        .int => |int| try self.print(file_writer, "{}", .{int}),
        .uint => |uint| try self.print(file_writer, "{}", .{uint}),
        .string => |string| try self.print(file_writer, "\"{s}\"", .{string}),
        .char => |char| try self.print(file_writer, "'{c}'", .{char}),
        .call => |c| try call(self, file_writer, c),
        .member => |m| try member(self, file_writer, m),
        .range => std.debug.print("illegal range expression\n", .{}),
        .prefix => |prefix| {
            try self.write(file_writer, switch (prefix.op) {
                .dash => "-",
                .bang => "!",
            });
            try compile(self, file_writer, prefix.rhs, .{});
        },
        .ident => |ident| {
            if (self.getSymbolType(ident) catch null) |_|
                try self.write(file_writer, ident)
            else
                std.debug.panic("comperr: unknown symbol {s}\n", .{ident});
        },
        .struct_instantiation => |struct_inst| {
            try self.print(file_writer, "({s}){{\n", .{struct_inst.name});
            self.indent_level += 1;

            var members = struct_inst.members.iterator();
            while (members.next()) |m| {
                try self.indent(file_writer);
                try self.print(file_writer, ".{s} = ", .{m.key_ptr.*});
                try compile(self, file_writer, m.value_ptr, .{});
                try self.write(file_writer, ",\n");
            }

            self.indent_level -= 1;
            try self.indent(file_writer);
            try self.write(file_writer, "}");
        },
        .reference => |reference| {
            try self.write(file_writer, "&");
            try compile(self, file_writer, reference.inner, .{});
        },
        .array_instantiation => |array| {
            try self.write(file_writer, "(");
            if (opts.is_const) try self.write(file_writer, "const ");
            try self.compileType(file_writer, try .fromAst(self, array.type));
            try self.print(file_writer, "[]){{", .{});
            for (array.contents.items, 1..) |*item, i| {
                try compile(self, file_writer, item, .{});
                if (i < array.contents.items.len) try self.write(file_writer, ", ");
            }
            try self.write(file_writer, "}");
        },
        .index => |index| {
            try compile(self, file_writer, index.lhs, .{});
            try self.write(file_writer, "[");
            try compile(self, file_writer, index.index, .{});
            try self.write(file_writer, "]");
        },
        .@"if" => |@"if"| {
            try self.write(file_writer, "((");
            try compile(self, file_writer, @"if".condition, .{});
            try self.write(file_writer, ") ? ");

            if (@"if".capture != null)
                std.debug.print("unimplemented if expression capture\n", .{});

            try compile(self, file_writer, @"if".body, .{});

            if (@"if".@"else") |@"else"| {
                try self.write(file_writer, " : ");
                try compile(self, file_writer, @"else", .{});
                try self.write(file_writer, ")");
            } else std.debug.panic("comperr: if expression must contain an else clause\n", .{});
        },
        else => |other| std.debug.print("unimplemented expression {s}\n", .{@tagName(other)}),
    }
}

fn member(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    expr: ast.Expression.Member,
) CompilerError!void {
    const parent_type: Type = try .infer(self, expr.parent.*);
    var delimiter: enum { @".", @"->" } = .@".";
    b: switch (parent_type) {
        .@"struct" => |@"struct"| {
            if (@"struct".getProperty(expr.member_name)) |property| switch (property) {
                .member => {
                    try compile(self, file_writer, expr.parent, .{});
                    try self.print(file_writer, "{s}{s}", .{
                        @tagName(delimiter),
                        expr.member_name,
                    });
                    delimiter = .@".";
                },
                .method => |method| try self.print(file_writer, "&{s}", .{method.inner_name}),
            } else std.debug.panic("comperr: property {s} doesn't exist for type {s}\n", .{
                expr.member_name,
                @"struct".name,
            });
        },
        .reference => |reference| {
            delimiter = .@"->";
            continue :b reference.inner.*;
        },
        else => |other| std.debug.panic(
            "comperr: member expression on {s} is illegal\n",
            .{@tagName(other)},
        ),
    }
}

fn assignment(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    expr: ast.Expression.Assignment,
) CompilerError!void {
    try compile(self, file_writer, expr.assignee, .{});
    try self.print(file_writer, " {s} ", .{switch (expr.op) {
        .and_equals => "&=",
        .minus_equals => "-=",
        .mod_equals => "%=",
        .or_equals => "|=",
        .plus_equals => "+=",
        .shift_left_equals => "<<=",
        .shift_right_equals => ">>=",
        .slash_equals => "/=",
        .times_equals => "*=",
        .xor_equals => "^=",
        .equals => "=",
    }});
    try compile(self, file_writer, expr.value, .{});
}

fn binary(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    expr: ast.Expression.Binary,
) CompilerError!void {
    try compile(self, file_writer, expr.lhs, .{});
    try self.print(file_writer, " {s} ", .{switch (expr.op) {
        .plus => "+",
        .dash => "-",
        .asterisk => "*",
        .slash => "/",
        .percent => "%",

        .equals_equals => "==",
        .greater => ">",
        .less => "<",
        .greater_equals => ">=",
        .less_equals => "<=",
        .bang_equals => "!=",

        .ampersand => "&",
        .pipe => "|",
        .caret => "^",
        .logical_and => "&&",
        .logical_or => "||",
        .shift_right => ">>",
        .shift_left => "<<",
    }});
    try compile(self, file_writer, expr.rhs, .{});
}

fn call(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    call_expr: ast.Expression.Call,
) CompilerError!void {
    switch (call_expr.callee.*) {
        .member => |m| {
            var parent_reference_level: i32 = 0;
            var reference_is_mut = switch (m.parent.*) {
                .ident => |ident| try self.getSymbolMutability(ident),
                else => false,
            };

            b: switch (try Type.infer(self, m.parent.*)) {
                .@"struct" => |@"struct"| if (@"struct".methods.get(m.member_name)) |method| {
                    switch (method.params.items[0].*) {
                        .reference => |param_arg| if (param_arg.is_mut and !reference_is_mut) std.debug.panic(
                            "comperr: method requires &mut {s}, but &{s} was passed.",
                            .{ @"struct".name, @"struct".name },
                        ),
                        .@"struct" => |param_arg| if (!param_arg.eq(&@"struct")) std.debug.panic("comperr: idk\n", .{}),
                        else => unreachable,
                    }

                    for (method.params.items[1..], 0..) |param, i| {
                        if (!param.eq(&try .infer(self, call_expr.args.items[i])))
                            std.debug.panic("comperr: type doesn't match method signature\n", .{});
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
                    try self.print(file_writer, "{s}(", .{method.inner_name});
                    for (0..@abs(ref_level_diff)) |_|
                        try self.write(
                            file_writer,
                            if (ref_level_diff > 0) "*" else if (ref_level_diff < 0)
                                "&"
                            else
                                unreachable,
                        );
                    try compile(self, file_writer, m.parent, .{});
                    try self.write(file_writer, ", ");
                    for (call_expr.args.items, 1..) |*arg, i| {
                        try compile(self, file_writer, arg, .{});
                        if (i < call_expr.args.items.len) try self.write(file_writer, ", ");
                    }
                    try self.write(file_writer, ")");
                } else std.debug.panic("comperr: {s}.{s} is not a method\n", .{
                    @"struct".name,
                    m.member_name,
                }),
                .reference => |reference| {
                    if (reference.is_mut) reference_is_mut = true;

                    parent_reference_level += 1;
                    continue :b reference.inner.*;
                },
                else => |other| std.debug.panic(
                    "comperr: member expression on {s} is illegal\n",
                    .{@tagName(other)},
                ),
            }
        },
        else => switch (try Type.infer(self, call_expr.callee.*)) {
            .function => |function| {
                for (function.params.items[1..], 0..) |param, i| {
                    std.debug.print("param type: {}\n", .{param});
                    std.debug.print("infer type: {}\n", .{try Type.infer(self, call_expr.args.items[i])});
                    if (!param.eq(&try .infer(self, call_expr.args.items[i]))) {
                        std.debug.panic("comperr: type doesn't match function signature\n", .{});
                    }
                }

                try compile(self, file_writer, call_expr.callee, .{});
                try self.write(file_writer, "(");
                for (call_expr.args.items, 1..) |*e, i| {
                    try compile(self, file_writer, e, .{});
                    if (i < call_expr.args.items.len) try self.write(file_writer, ", ");
                }
                try self.write(file_writer, ")");
            },
            else => std.debug.panic("comperr: expression is not callable\n", .{}),
        },
    }
}
