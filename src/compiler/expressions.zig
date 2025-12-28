const std = @import("std");

const ast = @import("Parser").ast;

const Self = @import("Compiler.zig");
const Type = @import("Type.zig").Type;

const CompilerError = Self.CompilerError;

pub fn compileExpression(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    expression: *const ast.Expression,
) CompilerError!void {
    switch (expression.*) {
        .assignment => |assignment| {
            try compileExpression(self, file_writer, assignment.assignee);
            try self.print(file_writer, " {s} ", .{switch (assignment.op) {
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
            try compileExpression(self, file_writer, assignment.value);
        },
        .block => |block| try self.compileBlock(file_writer, block),
        .binary => |binary| {
            try compileExpression(self, file_writer, binary.lhs);
            try self.print(file_writer, " {s} ", .{switch (binary.op) {
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
            try compileExpression(self, file_writer, binary.rhs);
        },
        .float => |float| try self.print(file_writer, "{}", .{float}),
        .int => |int| try self.print(file_writer, "{}", .{int}),
        .uint => |uint| try self.print(file_writer, "{}", .{uint}),
        .string => |string| try self.print(file_writer, "\"{s}\"", .{string}),
        .char => |char| try self.print(file_writer, "'{c}'", .{char}),
        .prefix => |prefix| {
            try self.write(file_writer, switch (prefix.op) {
                .dash => "-",
                .bang => "!",
            });
            try compileExpression(self, file_writer, prefix.rhs);
        },
        .call => |call| switch (call.callee.*) {
            .member => |member| {
                var parent_expr_buf: std.ArrayList(u8) = .empty;
                try compileExpression(self, &parent_expr_buf, member.parent);
                const parent_type = try self.getSymbolType(parent_expr_buf.items);

                var parent_reference_level: i32 = 0;

                b: switch (parent_type) {
                    .@"struct" => |@"struct"| {
                        if (@"struct".methods.get(member.member_name)) |method| {
                            const self_param = method.params.items[0];
                            const ref_level_diff = parent_reference_level - b2: {
                                var self_method_reference_level: i32 = 0;
                                count_ref_level: switch (self_param.*) {
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
                            try compileExpression(self, file_writer, member.parent);
                            try self.write(file_writer, ", ");
                            for (call.args.items, 1..) |*arg, i| {
                                try compileExpression(self, file_writer, arg);
                                if (i < call.args.items.len) try self.write(file_writer, ", ");
                            }
                            try self.write(file_writer, ")");
                        } else std.debug.panic("comperr: {s}.{s} is not a method\n", .{
                            @"struct".name,
                            member.member_name,
                        });
                    },
                    .reference => |reference| {
                        parent_reference_level += 1;
                        continue :b reference.inner.*;
                    },
                    else => |other| std.debug.panic(
                        "comperr: member expression on {s} is illegal\n",
                        .{@tagName(other)},
                    ),
                }
            },
            else => {
                try compileExpression(self, file_writer, call.callee);
                try self.write(file_writer, "(");
                for (call.args.items, 1..) |*expr, i| {
                    try compileExpression(self, file_writer, expr);
                    if (i < call.args.items.len) try self.write(file_writer, ", ");
                }
                try self.write(file_writer, ")");
            },
        },
        .member => |member| try compileMemberExpression(self, file_writer, member),
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
            while (members.next()) |member| {
                try self.indent(file_writer);
                try self.print(file_writer, ".{s} = ", .{member.key_ptr.*});
                try compileExpression(self, file_writer, member.value_ptr);
                try self.write(file_writer, ",\n");
            }

            self.indent_level -= 1;
            try self.indent(file_writer);
            try self.write(file_writer, "}");
        },
        .range => std.debug.print("illegal range expression\n", .{}),
        .reference => |reference| {
            try self.write(file_writer, "&");
            try compileExpression(self, file_writer, reference.inner);
        },
        else => |other| std.debug.print("unimplemented expression {s}\n", .{@tagName(other)}),
    }
}

fn compileMemberExpression(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    member: ast.Expression.Member,
) CompilerError!void {
    const parent_type: Type = try .infer(self, member.parent.*);
    var delimiter: enum { @".", @"->" } = .@".";
    b: switch (parent_type) {
        .@"struct" => |@"struct"| {
            if (@"struct".getProperty(member.member_name)) |property| switch (property) {
                .member => {
                    try compileExpression(self, file_writer, member.parent);
                    try self.print(file_writer, "{s}{s}", .{
                        @tagName(delimiter),
                        member.member_name,
                    });
                    delimiter = .@".";
                },
                .method => std.debug.print("unimplemented: member methods\n", .{}),
            } else std.debug.panic("comperr: property {s} doesn't exist for type {s}\n", .{
                member.member_name,
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
