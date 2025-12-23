const std = @import("std");

const utils = @import("../utils.zig");
const ast = @import("../parser/ast.zig");
const types = @import("types.zig");

const Parser = @import("../parser/Parser.zig");

const Self = @This();

pub const CompilerError = error{
    UnsupportedType,
    UnsupportedExpression,
    UndeclaredVariable,
    UnknownSymbol,
    UndeclaredType,
    UndeclaredField,
    VariableRedeclaration,
    OutOfMemory,
    AssignmentToImmutableVariable,
    MemberExpressionOnPrimitiveType,
} || Parser.ParserError || std.Io.Writer.Error;

alloc: std.mem.Allocator,
parser: *const Parser,

output_file: std.fs.File,
output_writer: std.fs.File.Writer,
output_buf: [1024]u8 = undefined,

indent_level: usize = 0,

/// stack of scopes.
scopes: std.ArrayList(Scope) = .empty,

/// maps a symbol name to the symbol's type
const Scope = std.StringHashMap(ast.Type);

pub fn init(alloc: std.mem.Allocator, parser: *const Parser, file_path: []const u8) !*Self {
    const self = try alloc.create(Self);

    const out_path = try std.fs.path.join(alloc, &.{ ".dmr-out", std.fs.path.dirname(file_path) orelse "" });
    var dmr_out = try std.fs.cwd().makeOpenPath(out_path, .{});
    defer dmr_out.close();

    const out_file_path = try std.fmt.allocPrint(alloc, "{s}.c", .{std.fs.path.basename(file_path)});
    defer alloc.free(out_file_path);
    const output_file = try dmr_out.createFile(out_file_path, .{});

    self.* = .{
        .alloc = alloc,
        .parser = parser,

        .output_file = output_file,
        .output_writer = output_file.writer(&self.output_buf),
    };

    try self.pushScope();

    return self;
}

fn write(self: *Self, comptime fmt: []const u8, args: anytype) CompilerError!void {
    try self.output_writer.interface.print(fmt, args);
}

/// prints 4 spaces for each indent level
inline fn writeIndent(self: *Self) CompilerError!void {
    for (0..self.indent_level) |_|
        try self.output_writer.interface.print("    ", .{});
}

fn writeBytes(self: *Self, bytes: []const u8) CompilerError!void {
    try self.output_writer.interface.writeAll(bytes);
}

pub fn deinit(self: *Self) void {
    self.output_file.close();

    self.popScope();
    // there should be no more scopes left
    std.debug.assert(self.scopes.items.len == 0);

    for (self.scopes.items) |*scope| scope.deinit();
    self.scopes.deinit(self.alloc);
}

/// Entry point for the compiler. Compiles AST into C code.
pub fn emit(self: *Self) CompilerError!void {
    for (self.parser.output.items) |statement|
        try self.compileStatement(statement);
}

fn compileStatement(self: *Self, statement: ast.Statement) CompilerError!void {
    switch (statement) {
        .function_definition => |fn_def| try self.compileFunctionDefinition(fn_def),
        .struct_declaration => |struct_decl| try self.compileStructDeclaration(struct_decl),
        .@"return" => |return_expr| try self.compileReturnStatement(return_expr),
        .variable_definition => |var_decl| try self.compileVariableDefinition(var_decl),
        .expression => |*expr| _ = try self.compileExpression(expr),
        // .@"if" => |if_stmt| try self.compileIfStatement(if_stmt),
        // .@"while" => |while_stmt| try self.compileWhileStatement(while_stmt),
        // .@"for" => |for_stmt| try self.compileForStatement(for_stmt),
        .block => |block| try self.compileBlock(block),
        else => std.debug.print("unimplemented statement {s}\n", .{@tagName(statement)}), // TODO: implement all statement types
    }

    try self.output_writer.interface.flush();
}

fn compileStructDeclaration(self: *Self, struct_decl: ast.Statement.StructDeclaration) CompilerError!void {
    try self.registerSymbol(struct_decl.name, .{ .symbol = struct_decl.name });

    try self.write("struct {s} {{\n", .{struct_decl.name});
    self.indent_level += 1;

    for (struct_decl.members.items) |member| {
        try self.writeIndent();
        try self.compileVariableSignature(member.name, member.type);
        try self.writeBytes(";\n");
    }

    if (struct_decl.methods.items.len != 0) {
        std.debug.print("unimplemented type: struct method\n", .{});
    }

    self.indent_level -= 1;
    try self.write("}};\n\n", .{});
}

fn compileFunctionDefinition(self: *Self, function_def: ast.FunctionDefinition) CompilerError!void {
    try self.registerSymbol(function_def.name, .{ .function = function_def.getType() });

    try self.write("{s}", .{try self.compileType(function_def.return_type)});
    try self.write(" {s}(", .{function_def.name});
    for (function_def.parameters.items, 1..) |parameter, i| {
        try self.compileVariableSignature(parameter.name, parameter.type);
        if (i < function_def.parameters.items.len) try self.writeBytes(", ");
    }
    try self.writeBytes(") ");

    try self.compileBlock(function_def.body);
}

fn compileBlock(self: *Self, block: ast.Block) CompilerError!void {
    try self.pushScope();
    defer self.popScope();

    try self.writeBytes("{\n");

    self.indent_level += 1;
    for (block.items) |statement| {
        try self.writeIndent();
        try self.compileStatement(statement);
    }
    self.indent_level -= 1;

    try self.writeBytes("}\n\n");
}

/// prints the type to the file
fn compileType(self: *Self, t: ast.Type) CompilerError![]const u8 {
    _ = self;
    return switch (t) {
        .symbol => |symbol| types.get(symbol) catch symbol,
        else => |other| std.debug.panic("unimplemented type {s}\n", .{@tagName(other)}),
    };
}

fn inferType(self: *Self, expr: ast.Expression) !ast.Type {
    return switch (expr) {
        .ident => |ident| .{ .symbol = ident },
        .int => .{ .symbol = "i32" },
        .float => .{ .symbol = "f32" },
        .char => .{ .symbol = "u8" },
        .struct_instantiation => |struct_inst| .{
            .symbol = try self.compileType(try self.getSymbolType(struct_inst.name)),
        },
        else => |other| std.debug.panic("unimplemented type: {s}\n", .{@tagName(other)}),
    };
}

fn compileVariableSignature(self: *Self, name: []const u8, @"type": ast.Type) CompilerError!void {
    try self.write("{s}", .{try self.compileType(@"type")});
    try self.write(" {s}", .{name});
}

fn compileVariableDefinition(self: *Self, v: ast.Statement.VariableDefinition) CompilerError!void {
    if (!v.is_mut) try self.writeBytes("const ");

    if (v.type == .inferred) {
        const t = try self.inferType(v.assigned_value);
        try self.compileVariableSignature(v.variable_name, t);
    } else {
        try self.compileVariableSignature(v.variable_name, v.type);
    }

    try self.writeBytes(" = ");

    try self.compileExpression(&v.assigned_value);

    try self.writeBytes(";\n");

    try self.registerSymbol(v.variable_name, v.type);
}

fn compileReturnStatement(self: *Self, r: ?ast.Expression) CompilerError!void {
    try self.writeBytes("return");
    if (r) |*expression| {
        try self.writeBytes(" ");
        try self.compileExpression(expression);
    }
    try self.writeBytes(";\n");
}

fn compileExpression(self: *Self, expression: *const ast.Expression) CompilerError!void {
    switch (expression.*) {
        .assignment => |assignment| {
            try self.compileExpression(assignment.assignee);
            try self.writeBytes(switch (assignment.op) {
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
            });
            try self.compileExpression(assignment.value);
        },
        .block => |block| try self.compileBlock(block),
        .binary => |binary| {
            try self.compileExpression(binary.lhs);
            try self.writeBytes(switch (binary.op) {
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
            });
            try self.compileExpression(binary.rhs);
        },
        .float => |float| try self.write("{}", .{float}),
        .int => |int| try self.write("{}", .{int}),
        .uint => |uint| try self.write("{}", .{uint}),
        .string => |string| try self.write("\"{s}\"", .{string}),
        .char => |char| try self.write("'{c}'", .{char}),
        .prefix => |prefix| {
            try self.writeBytes(switch (prefix.op) {
                .dash => "-",
            });
            try self.compileExpression(prefix.rhs);
        },
        .call => |call| {
            try self.compileExpression(call.callee);
            try self.writeBytes("(");
            for (call.args.items, 1..) |*expr, i| {
                try self.compileExpression(expr);
                if (i < call.args.items.len) try self.writeBytes(", ");
            }
            try self.writeBytes(")");
        },
        .member => |member| {
            try self.compileExpression(member.lhs);
            try self.writeBytes("."); // TODO: or -> if lhs is a pointer
            try self.compileExpression(member.rhs);
        },
        .ident => |ident| try self.write("{s}", .{ident}),
        .struct_instantiation => |struct_inst| {
            try self.write("({s}){{\n", .{struct_inst.name});
            self.indent_level += 1;

            var members = struct_inst.members.iterator();
            while (members.next()) |member| {
                try self.writeIndent();
                try self.write(".{s} = ", .{member.key_ptr.*});
                try self.compileExpression(member.value_ptr);
                try self.writeBytes(",\n");
            }

            self.indent_level -= 1;
            try self.writeIndent();
            try self.writeBytes("}");
        },
        else => |other| std.debug.print("unimplemented expression {s}\n", .{@tagName(other)}),
    }
}

/// appends a new empty scope to the scope stack.
fn pushScope(self: *Self) !void {
    try self.scopes.append(self.alloc, .init(self.alloc));
}

/// pops the scope of the scope stack.
fn popScope(self: *Self) void {
    var last = self.scopes.pop().?;
    last.deinit();
}

/// registers a new entry in the top scope of the scope stack.
fn registerSymbol(self: *Self, name: []const u8, @"type": ast.Type) !void {
    var last = &self.scopes.items[self.scopes.items.len - 1];
    try last.put(name, @"type");
}

fn getSymbolType(self: *const Self, symbol: []const u8) !ast.Type {
    var it = std.mem.reverseIterator(self.scopes.items);
    while (it.next()) |scope|
        return scope.get(symbol) orelse
            continue;

    return error.UnknownSymbol;
}
