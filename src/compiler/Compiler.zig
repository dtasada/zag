const std = @import("std");

const utils = @import("../utils.zig");
const ast = @import("../parser/ast.zig");

const Parser = @import("../parser/Parser.zig");

const Type = @import("Type.zig").Type;
const Value = @import("Value.zig").Value;

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

output: File,
zag_header: File,

indent_level: usize = 0,

/// stack of scopes.
scopes: std.ArrayList(Scope) = .empty,

const File = struct {
    handler: std.fs.File,
    writer: std.fs.File.Writer,
    buf: []u8,

    fn init(alloc: std.mem.Allocator, file: std.fs.File) !File {
        var self: File = .{
            .handler = file,
            .buf = try alloc.alloc(u8, 1024),
            .writer = undefined,
        };
        self.writer = self.handler.writer(self.buf);
        return self;
    }

    fn deinit(self: *File, alloc: std.mem.Allocator) void {
        self.handler.close();
        alloc.free(self.buf);
    }

    fn write(self: *File, bytes: []const u8) !void {
        _ = try self.writer.interface.write(bytes);
    }

    fn flush(self: *File) !void {
        try self.writer.interface.flush();
    }
};

/// maps a symbol name to the symbol's type
const Scope = std.StringHashMap(union(enum) {
    const Item = struct {
        type: Type,
        inner_name: []const u8,
    };

    symbol: Item,
    type: Item,
});

pub fn init(alloc: std.mem.Allocator, parser: *const Parser, file_path: []const u8) !*Self {
    const self = try alloc.create(Self);

    const out_path = try std.fs.path.join(alloc, &.{ ".zag-out", std.fs.path.dirname(file_path) orelse "" });
    var zag_out = try std.fs.cwd().makeOpenPath(out_path, .{});
    defer zag_out.close();

    const out_file_path = try std.fmt.allocPrint(alloc, "{s}.c", .{std.fs.path.basename(file_path)});
    defer alloc.free(out_file_path);
    const output_file = try zag_out.createFile(out_file_path, .{});

    const zag_header_path = try std.fmt.allocPrint(alloc, "zag.h", .{});
    defer alloc.free(zag_header_path);
    const zag_header_file = try zag_out.createFile(zag_header_path, .{});

    self.* = .{
        .alloc = alloc,
        .parser = parser,

        .output = try .init(alloc, output_file),
        .zag_header = try .init(alloc, zag_header_file),
    };

    try self.pushScope();

    return self;
}

pub fn deinit(self: *Self) void {
    self.output.deinit(self.alloc);
    self.zag_header.deinit(self.alloc);

    self.popScope();
    // there should be no more scopes left
    std.debug.assert(self.scopes.items.len == 0);

    for (self.scopes.items) |*scope| scope.deinit();
    self.scopes.deinit(self.alloc);
}

fn print(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    comptime fmt: []const u8,
    args: anytype,
) CompilerError!void {
    try file_writer.print(self.alloc, fmt, args);
}

fn write(self: *Self, file_writer: *std.ArrayList(u8), bytes: []const u8) CompilerError!void {
    try file_writer.appendSlice(self.alloc, bytes);
}

/// prints 4 spaces for each indent level into an arraylist
inline fn indent(self: *Self, file: *std.ArrayList(u8)) CompilerError!void {
    for (0..self.indent_level) |_|
        try file.appendSlice(self.alloc, "    ");
}

/// Entry point for the compiler. Compiles AST into C code.
pub fn emit(self: *Self) CompilerError!void {
    var file_writer: std.ArrayList(u8) = .empty;

    try self.write(&file_writer, "#include <zag.h>\n");

    for (self.parser.output.items) |*statement|
        try self.compileStatement(&file_writer, statement);

    try self.output.write(file_writer.items);
    try self.output.flush();
}

fn compileStatement(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    statement: *const ast.Statement,
) CompilerError!void {
    switch (statement.*) {
        .function_definition => |fn_def| try self.compileFunctionDefinition(file_writer, fn_def),
        .struct_declaration => |struct_decl| try self.compileCompoundTypeDeclaration(file_writer, .@"struct", struct_decl),
        .@"return" => |return_expr| try self.compileReturnStatement(file_writer, return_expr),
        .variable_definition => |var_decl| try self.compileVariableDefinition(file_writer, var_decl),
        .expression => |*expr| {
            try self.compileExpression(file_writer, expr);
            try self.write(file_writer, ";\n");
        },
        .@"if" => |if_stmt| try self.compileConditionalStatement(file_writer, .@"if", if_stmt),
        .@"while" => |while_stmt| try self.compileConditionalStatement(file_writer, .@"while", while_stmt),
        .@"for" => |for_stmt| try self.compileConditionalStatement(file_writer, .@"for", for_stmt),
        .block => |block| try self.compileBlock(file_writer, block),
        .enum_declaration => |enum_decl| try self.compileCompoundTypeDeclaration(file_writer, .@"enum", enum_decl),
        .union_declaration => |union_decl| try self.compileCompoundTypeDeclaration(file_writer, .@"union", union_decl),
    }
}

fn compileCompoundTypeDeclaration(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    comptime T: enum { @"struct", @"union", @"enum" },
    type_decl: switch (T) {
        .@"struct" => ast.Statement.StructDeclaration,
        .@"union" => ast.Statement.UnionDeclaration,
        .@"enum" => ast.Statement.EnumDeclaration,
    },
) CompilerError!void {
    // register struct in scope
    var compound_type: switch (T) {
        .@"struct" => Type.Struct,
        .@"union" => Type.Union,
        .@"enum" => Type.Enum,
    } = .{
        .name = type_decl.name,
        .members = .init(self.alloc),
        .methods = .init(self.alloc),
    };
    try self.registerSymbol(
        type_decl.name,
        switch (T) {
            .@"struct" => .{ .@"struct" = compound_type },
            .@"union" => .{ .@"union" = compound_type },
            .@"enum" => .{ .@"enum" = compound_type },
        },
        .type,
    );
    for (type_decl.members.items) |member|
        try compound_type.members.put(member.name, switch (T) {
            // TODO: default values
            .@"struct" => b: {
                const member_type = try self.alloc.create(Type);
                member_type.* = try self.getTypeFromAst(.{ .strong = member.type });
                break :b member_type;
            },
            .@"union" => b: {
                const member_type = try self.alloc.create(Type);
                member_type.* = if (member.type) |t|
                    try self.getTypeFromAst(.{ .strong = t })
                else
                    .void;
                break :b member_type;
            },
            .@"enum" => b: {
                std.debug.print("unimplemented enum explicit values\n", .{});
                break :b null;
            },
        });

    for (type_decl.methods.items) |method| {
        var params: std.ArrayList(*const Type) = try .initCapacity(
            self.alloc,
            method.parameters.items.len,
        );

        for (method.parameters.items) |p| {
            const param_type = try self.alloc.create(Type);
            param_type.* = try self.getTypeFromAst(.{ .strong = p.type });
            params.appendAssumeCapacity(param_type);
        }

        const return_type = try self.alloc.create(Type);
        return_type.* = try self.getTypeFromAst(.{ .strong = method.return_type });
        try compound_type.methods.put(method.name, .{
            .params = params,
            .return_type = return_type,
        });
    }

    try self.print(file_writer, "typedef {s} {{\n", .{switch (T) {
        .@"struct" => "struct",
        .@"union" => "union",
        .@"enum" => "enum",
    }});
    self.indent_level += 1;

    for (type_decl.members.items) |member| {
        try self.indent(file_writer);
        switch (T) {
            .@"struct" => {
                try self.compileVariableSignature(
                    file_writer,
                    member.name,
                    try self.getTypeFromAst(.{ .strong = member.type }),
                );
                try self.write(file_writer, ";\n");
            },
            .@"union" => {
                try self.compileVariableSignature(
                    file_writer,
                    member.name,
                    if (member.type) |t|
                        try self.getTypeFromAst(.{ .strong = t })
                    else
                        .void,
                );
                try self.write(file_writer, ";\n");
            },
            .@"enum" => {
                try self.print(file_writer, "{s},\n", .{member.name});
                std.debug.print("unimplemented explicit enum member values\n", .{});
            },
        }
    }

    self.indent_level -= 1;
    try self.print(file_writer, "}} {s};\n\n", .{type_decl.name});

    for (type_decl.methods.items) |method| {
        try self.registerSymbol(method.name, try self.getTypeFromAst(.{ .strong = method.getType() }), .symbol);
        try self.pushScope();
        defer self.popScope();

        try self.compileTypeAst(file_writer, method.return_type);
        try self.print(file_writer, " __zag_{s}_{s}(", .{ type_decl.name, method.name }); // TODO: generics
        for (method.parameters.items, 1..) |parameter, i| {
            const parameter_type = try self.getTypeFromAst(.{ .strong = parameter.type });
            try self.registerSymbol(parameter.name, parameter_type, .symbol);
            try self.compileVariableSignature(file_writer, parameter.name, parameter_type);
            if (i < method.parameters.items.len) try self.write(file_writer, ", ");
        }
        try self.write(file_writer, ") ");

        try self.compileBlock(file_writer, method.body);
    }
}

fn compileFunctionDefinition(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    function_def: ast.FunctionDefinition,
) CompilerError!void {
    try self.registerSymbol(function_def.name, try self.getTypeFromAst(.{ .strong = function_def.getType() }), .symbol);

    try self.compileTypeAst(file_writer, function_def.return_type);
    try self.print(file_writer, " {s}(", .{function_def.name});
    for (function_def.parameters.items, 1..) |parameter, i| {
        try self.compileVariableSignature(file_writer, parameter.name, try self.getTypeFromAst(.{ .strong = parameter.type }));
        if (i < function_def.parameters.items.len) try self.write(file_writer, ", ");
    }
    try self.write(file_writer, ") ");

    try self.compileBlock(file_writer, function_def.body);
}

fn compileConditionalStatement(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    comptime T: enum { @"if", @"while", @"for" },
    statement: switch (T) {
        .@"if" => ast.Statement.If,
        .@"while" => ast.Statement.While,
        .@"for" => ast.Statement.For,
    },
) CompilerError!void {
    try self.print(file_writer, "{s} (", .{switch (T) {
        .@"if" => "if",
        .@"for" => "for",
        .@"while" => "while",
    }});

    switch (T) {
        .@"if", .@"while" => try self.compileExpression(file_writer, statement.condition),
        .@"for" => switch (statement.iterator.*) {
            .range => |range| {
                try self.compileType(file_writer, try self.inferType(range.start.*));
                try self.print(file_writer, " {s} = ", .{statement.capture});
                try self.compileExpression(file_writer, range.start);
                try self.print(file_writer, "; {s} < ", .{statement.capture});
                try self.compileExpression(file_writer, range.end);
                try self.print(file_writer, "; {s}++", .{statement.capture});
            },
            else => |other| switch (try self.inferType(other)) {
                .array => std.debug.print("unimplemented array iterator in for loop\n", .{}),
                else => std.debug.print("illegal array iterator type\n", .{}),
            },
        },
    }
    try self.write(file_writer, ") ");

    switch (T) {
        .@"if", .@"while" => if (statement.capture != null)
            std.debug.print("unimplemented conditional statement capture\n", .{}),
        else => {},
    }

    try self.compileStatement(file_writer, statement.body);

    switch (T) {
        .@"if" => if (statement.@"else") |@"else"|
            try self.compileStatement(file_writer, @"else"),
        else => {},
    }
}

fn compileBlock(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    block: ast.Block,
) CompilerError!void {
    try self.pushScope();
    defer self.popScope();

    try self.write(file_writer, "{\n");

    self.indent_level += 1;
    for (block.items) |*statement| {
        try self.indent(file_writer);
        try self.compileStatement(file_writer, statement);
    }
    self.indent_level -= 1;

    try self.indent(file_writer);
    try self.write(file_writer, "}\n\n");
}

fn compileTypeAst(self: *Self, file_writer: *std.ArrayList(u8), t: ast.Type) CompilerError!void {
    switch (t) {
        .symbol => |symbol| try self.compileType(file_writer, try self.getSymbolType(symbol)),
        .reference => |reference| {
            try self.compileTypeAst(file_writer, reference.inner.*);
            try self.print(file_writer, " *{s}", .{if (reference.is_mut) "" else " const"});
        },
        else => |other| std.debug.panic("unimplemented type {s}\n", .{@tagName(other)}),
    }
}

fn compileType(self: *Self, file_writer: *std.ArrayList(u8), t: Type) CompilerError!void {
    return switch (t) {
        .reference => |reference| {
            try self.compileType(file_writer, reference.inner.*);
            try self.print(file_writer, " *{s}", .{if (reference.is_mut) "" else " const"});
        },
        .@"struct" => |s| try self.write(file_writer, try self.getInnerName(s.name)),
        .optional, .array, .error_union, .function => std.debug.panic("unimplemented type: {any}\n", .{t}),
        else => |primitive| try self.write(file_writer, @tagName(primitive)),
    };
}

/// Converts an AST type to a Compiler type.
/// `infer_expr` is the expression with which the type is inferred.
fn getTypeFromAst(self: *Self, t: union(enum) {
    strong: ast.Type,
    infer: ast.Expression,
}) CompilerError!Type {
    return switch (t) {
        .strong => |strong| switch (strong) {
            .symbol => |symbol| Type.fromSymbol(symbol) catch try self.getSymbolType(symbol),
            .reference => |reference| .{
                .reference = .{
                    .inner = b: {
                        const ref_type = try self.alloc.create(Type);
                        ref_type.* = try self.getTypeFromAst(.{ .strong = reference.inner.* });
                        break :b ref_type;
                    },
                    .is_mut = reference.is_mut,
                },
            },
            .function => |function| .{
                .function = .{
                    .params = b: {
                        var params: std.ArrayList(*const Type) = try .initCapacity(self.alloc, function.parameters.items.len);
                        for (function.parameters.items) |p| {
                            const param = try self.alloc.create(Type);
                            param.* = try self.getTypeFromAst(.{ .strong = p.type });
                            params.appendAssumeCapacity(param);
                        }
                        break :b params;
                    },
                    .return_type = b: {
                        const return_type = try self.alloc.create(Type);
                        return_type.* = try self.getTypeFromAst(.{ .strong = function.return_type.* });
                        break :b return_type;
                    },
                },
            },
            .array => |array| .{
                .array = .{
                    .inner = b: {
                        const array_type = try self.alloc.create(Type);
                        array_type.* = try self.getTypeFromAst(.{ .strong = array.inner.* });
                        break :b array_type;
                    },
                    .size = (try self.solveComptimeExpression(if (array.size) |s|
                        s.*
                    else
                        @panic("can't infer array size"))).u64,
                },
            },
            else => |other| std.debug.panic("unimplemented type {s}\n", .{@tagName(other)}),
        },
        .infer => |expr| try self.inferType(expr),
    };
}

fn inferType(self: *Self, expr: ast.Expression) !Type {
    return switch (expr) {
        .ident => |ident| try self.getSymbolType(ident),
        .int => |int| if (int <= std.math.maxInt(i32)) .i32 else .i64,
        .uint => |uint| if (uint <= std.math.maxInt(i32)) .i32 else .i64,
        .float => .f32,
        .char => .u8,
        .struct_instantiation => |struct_inst| try self.getSymbolType(struct_inst.name),
        .prefix => |prefix| try self.inferType(prefix.rhs.*),
        .reference => |reference| .{
            .reference = .{
                .inner = b: {
                    const inner = try self.alloc.create(Type);
                    inner.* = try self.getTypeFromAst(.{ .infer = reference.inner.* });
                    break :b inner;
                },
                .is_mut = reference.is_mut,
            },
        },
        else => |other| std.debug.panic("unimplemented type: {s}\n", .{@tagName(other)}),
    };
}

fn compileVariableSignature(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    name: []const u8,
    @"type": Type,
) CompilerError!void {
    switch (@"type") {
        .array => |array| {
            if (array.size) |size| {
                try self.compileType(file_writer, array.inner.*);
                try self.print(file_writer, " {s}[{}]", .{ name, size });
            } else std.debug.print("unimplemented arraylist\n", .{});
        },
        else => {
            try self.compileType(file_writer, @"type");
            try self.print(file_writer, " {s}", .{name});
        },
    }
}

fn compileVariableDefinition(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    v: ast.Statement.VariableDefinition,
) CompilerError!void {
    const variable_type = try self.getTypeFromAst(
        if (v.type == .inferred)
            .{ .infer = v.assigned_value }
        else
            .{ .strong = v.type },
    );

    if (!v.is_mut) try self.write(file_writer, "const ");

    try self.compileVariableSignature(file_writer, v.variable_name, variable_type);

    try self.write(file_writer, " = ");

    try self.compileExpression(file_writer, &v.assigned_value);

    try self.write(file_writer, ";\n");

    try self.registerSymbol(v.variable_name, variable_type, .symbol);
}

fn compileReturnStatement(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    r: ?ast.Expression,
) CompilerError!void {
    try self.write(file_writer, "return");
    if (r) |*expression| {
        try self.write(file_writer, " ");
        try self.compileExpression(file_writer, expression);
    }
    try self.write(file_writer, ";\n");
}

fn compileExpression(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    expression: *const ast.Expression,
) CompilerError!void {
    switch (expression.*) {
        .assignment => |assignment| {
            try self.compileExpression(file_writer, assignment.assignee);
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
            try self.compileExpression(file_writer, assignment.value);
        },
        .block => |block| try self.compileBlock(file_writer, block),
        .binary => |binary| {
            try self.compileExpression(file_writer, binary.lhs);
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
            try self.compileExpression(file_writer, binary.rhs);
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
            try self.compileExpression(file_writer, prefix.rhs);
        },
        .call => |call| {
            try self.compileExpression(file_writer, call.callee);
            try self.write(file_writer, "(");
            for (call.args.items, 1..) |*expr, i| {
                try self.compileExpression(file_writer, expr);
                if (i < call.args.items.len) try self.write(file_writer, ", ");
            }
            try self.write(file_writer, ")");
        },
        .member => |member| {
            try self.compileExpression(file_writer, member.parent);
            try self.write(file_writer, if (try self.inferType(member.parent.*) == .reference) "->" else ".");
            try self.write(file_writer, member.member_name);
        },
        .ident => |ident| try self.write(file_writer, ident),
        .struct_instantiation => |struct_inst| {
            try self.print(file_writer, "({s}){{\n", .{struct_inst.name});
            self.indent_level += 1;

            var members = struct_inst.members.iterator();
            while (members.next()) |member| {
                try self.indent(file_writer);
                try self.print(file_writer, ".{s} = ", .{member.key_ptr.*});
                try self.compileExpression(file_writer, member.value_ptr);
                try self.write(file_writer, ",\n");
            }

            self.indent_level -= 1;
            try self.indent(file_writer);
            try self.write(file_writer, "}");
        },
        .range => std.debug.print("illegal range expression\n", .{}),
        .reference => |reference| {
            try self.write(file_writer, "&");
            try self.compileExpression(file_writer, reference.inner);
        },
        else => |other| std.debug.print("unimplemented expression {s}\n", .{@tagName(other)}),
    }
}

fn solveComptimeExpression(self: *Self, expression: ast.Expression) !Value {
    _ = self;
    return switch (expression) {
        .int => |int| .{ .i64 = int },
        .uint => |uint| .{ .u64 = uint },
        .float => |float| .{ .f64 = float },
        .char => |char| .{ .u8 = char },
        // .binary => |binary| try (try self.solveComptimeExpression(binary.lhs.*))
        //     .binaryOperation(binary.op, try self.solveComptimeExpression(binary.rhs.*)),
        else => std.debug.panic("unimplemented comptime expression for {s}\n", .{@tagName(expression)}),
    };
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
fn registerSymbol(
    self: *Self,
    name: []const u8,
    @"type": Type,
    symbol_or_type: enum { symbol, type },
) !void {
    var last = &self.scopes.items[self.scopes.items.len - 1];
    try last.put(name, switch (symbol_or_type) {
        .symbol => .{
            .symbol = .{
                .type = @"type",
                .inner_name = name, // TODO: name mangling for generics ig
            },
        },
        .type => .{
            .type = .{
                .type = @"type",
                .inner_name = name, // TODO: name mangling for generics ig
            },
        },
    });
}

fn getSymbolType(self: *const Self, symbol: []const u8) !Type {
    var it = std.mem.reverseIterator(self.scopes.items);
    while (it.next()) |scope|
        return switch (scope.get(symbol) orelse continue) {
            inline else => |s| s.type,
        };

    return Type.fromSymbol(symbol) catch utils.printErr(
        error.UnknownSymbol,
        "Compiler error: Unknown symbol: {s}\n",
        .{symbol},
        .red,
    );
}

fn getInnerName(self: *const Self, symbol: []const u8) ![]const u8 {
    var it = std.mem.reverseIterator(self.scopes.items);
    while (it.next()) |scope|
        return switch (scope.get(symbol) orelse continue) {
            inline else => |s| s.inner_name,
        };

    return utils.printErr(
        error.UnknownSymbol,
        "Compiler error: Unknown symbol: {s}\n",
        .{symbol},
        .red,
    );
}
