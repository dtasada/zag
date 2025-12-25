const std = @import("std");

const utils = @import("../utils.zig");
const ast = @import("../parser/ast.zig");

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
const Scope = std.StringHashMap(union(enum) {
    const Item = struct {
        type: Type,
        inner_name: []const u8,
    };

    symbol: Item,
    type: Item,
});

const Type = union(enum) {
    const Function = struct {
        params: std.ArrayList(*const Type),
        return_type: *const Type,
    };

    const Struct = struct {
        const Member = struct {
            name: []const u8,
            type: *const Type,
        };

        name: []const u8,
        members: std.ArrayList(Member),
        methods: std.ArrayList(Function),
    };

    const Reference = struct {
        inner: *const Type,
        is_mut: bool,
    };

    const Array = struct {
        inner: *const Type,
        /// if size is `null` type is an arraylist, else it's an array.
        /// if size is `_`, type is an array of inferred size.
        /// if size is a valid expression, type is an array of specified size.
        size: ?usize = null,
    };

    const ErrorUnion = struct {
        success: *const Type,
        @"error": ?*const Type = null,
    };

    i8,
    i16,
    i32,
    i64,

    u8,
    u16,
    u32,
    u64,

    f32,
    f64,

    void,

    @"struct": Struct,
    optional: *const Type,
    reference: Reference,
    array: Array,
    error_union: ErrorUnion,
    function: Function,

    fn fromSymbol(symbol: []const u8) !Type {
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
        else if (std.mem.eql(u8, symbol, "f32"))
            .f32
        else if (std.mem.eql(u8, symbol, "f64"))
            .f64
        else if (std.mem.eql(u8, symbol, "void"))
            .void
        else
            error.TypeNotPrimitive;
    }
};

pub fn init(alloc: std.mem.Allocator, parser: *const Parser, file_path: []const u8) !*Self {
    const self = try alloc.create(Self);

    const out_path = try std.fs.path.join(alloc, &.{ ".zag-out", std.fs.path.dirname(file_path) orelse "" });
    var zag_out = try std.fs.cwd().makeOpenPath(out_path, .{});
    defer zag_out.close();

    const out_file_path = try std.fmt.allocPrint(alloc, "{s}.c", .{std.fs.path.basename(file_path)});
    defer alloc.free(out_file_path);
    const output_file = try zag_out.createFile(out_file_path, .{});

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
    for (self.parser.output.items) |*statement|
        try self.compileStatement(statement);
}

fn compileStatement(self: *Self, statement: *const ast.Statement) CompilerError!void {
    switch (statement.*) {
        .function_definition => |fn_def| try self.compileFunctionDefinition(fn_def),
        .struct_declaration => |struct_decl| try self.compileStructDeclaration(struct_decl),
        .@"return" => |return_expr| try self.compileReturnStatement(return_expr),
        .variable_definition => |var_decl| try self.compileVariableDefinition(var_decl),
        .expression => |*expr| {
            try self.compileExpression(expr);
            try self.writeBytes(";\n");
        },
        .@"if" => |if_stmt| try self.compileIfStatement(if_stmt),
        .@"while" => |while_stmt| try self.compileWhileStatement(while_stmt),
        .@"for" => |for_stmt| try self.compileForStatement(for_stmt),
        .block => |block| try self.compileBlock(block),
        else => |other| std.debug.print("unimplemented statement {s}\n", .{@tagName(other)}), // TODO: implement all statement types
    }

    try self.output_writer.interface.flush();
}

fn compileStructDeclaration(
    self: *Self,
    struct_decl: ast.Statement.StructDeclaration,
) CompilerError!void {
    // register struct in scope
    try self.registerSymbol(struct_decl.name, .{
        .@"struct" = .{
            .name = struct_decl.name,
            .members = try .initCapacity(self.alloc, struct_decl.members.items.len),
            .methods = try .initCapacity(self.alloc, struct_decl.methods.items.len),
        },
    }, .type);
    const t = &self.scopes.getLast().getPtr(struct_decl.name).?.type.type.@"struct";
    for (struct_decl.members.items) |member| {
        // TODO: struct default values
        const member_type = try self.alloc.create(Type);
        member_type.* = try self.getTypeFromAst(.{ .strong = member.type });
        t.members.appendAssumeCapacity(.{
            .name = member.name,
            .type = member_type,
        });
    }

    for (struct_decl.methods.items) |method| {
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
        t.methods.appendAssumeCapacity(.{
            .params = params,
            .return_type = return_type,
        });
    }

    try self.write("typedef struct {{\n", .{});
    self.indent_level += 1;

    for (struct_decl.members.items) |member| {
        try self.writeIndent();
        try self.compileVariableSignature(member.name, try self.getTypeFromAst(.{ .strong = member.type }));
        try self.writeBytes(";\n");
    }

    self.indent_level -= 1;
    try self.write("}} {s};\n\n", .{struct_decl.name});

    for (struct_decl.methods.items) |method| {
        try self.registerSymbol(method.name, try self.getTypeFromAst(.{ .strong = method.getType() }), .symbol);
        try self.pushScope();
        defer self.popScope();

        try self.write("{s}", .{try self.compileTypeAst(method.return_type)});
        try self.write(" __zag_{s}_{s}(", .{ struct_decl.name, method.name }); // TODO: generics
        for (method.parameters.items, 1..) |parameter, i| {
            const parameter_type = try self.getTypeFromAst(.{ .strong = parameter.type });
            try self.registerSymbol(parameter.name, parameter_type, .symbol);
            try self.compileVariableSignature(parameter.name, parameter_type);
            if (i < method.parameters.items.len) try self.writeBytes(", ");
        }
        try self.writeBytes(") ");

        try self.compileBlock(method.body);
    }
}

fn compileFunctionDefinition(self: *Self, function_def: ast.FunctionDefinition) CompilerError!void {
    try self.registerSymbol(function_def.name, try self.getTypeFromAst(.{ .strong = function_def.getType() }), .symbol);

    try self.write("{s}", .{try self.compileTypeAst(function_def.return_type)});
    try self.write(" {s}(", .{function_def.name});
    for (function_def.parameters.items, 1..) |parameter, i| {
        try self.compileVariableSignature(parameter.name, try self.getTypeFromAst(.{ .strong = parameter.type }));
        if (i < function_def.parameters.items.len) try self.writeBytes(", ");
    }
    try self.writeBytes(") ");

    try self.compileBlock(function_def.body);
}

fn compileIfStatement(self: *Self, if_statement: ast.Statement.If) CompilerError!void {
    try self.writeBytes("if (");
    try self.compileExpression(if_statement.condition);
    try self.writeBytes(") ");

    try self.compileStatement(if_statement.body);
    if (if_statement.capture) |_|
        std.debug.print("unimplemented if statement capture\n", .{});

    if (if_statement.@"else") |@"else"|
        try self.compileStatement(@"else");
}

fn compileWhileStatement(self: *Self, while_statement: ast.Statement.While) CompilerError!void {
    try self.writeBytes("while (");
    try self.compileExpression(while_statement.condition);
    try self.writeBytes(") ");

    try self.compileStatement(while_statement.body);
    if (while_statement.capture) |_|
        std.debug.print("unimplemented while statement capture\n", .{});
}

fn compileForStatement(self: *Self, for_statement: ast.Statement.For) CompilerError!void {
    try self.writeBytes("for (");
    switch (for_statement.iterator.*) {
        .range => |range| {
            try self.write("{s} ", .{try self.compileType(try self.inferType(range.start.*))});
            try self.write("{s} = ", .{for_statement.capture});
            try self.compileExpression(range.start);
            try self.write("; {s} < ", .{for_statement.capture});
            try self.compileExpression(range.end);
            try self.write("; {s}++", .{for_statement.capture});
        },
        else => |other| switch (try self.inferType(other)) {
            .array => std.debug.print("unimplemented array iterator in for loop\n", .{}),
            else => std.debug.print("illegal array iterator type\n", .{}),
        },
    }
    try self.writeBytes(") ");

    try self.compileStatement(for_statement.body);
}

fn compileBlock(self: *Self, block: ast.Block) CompilerError!void {
    try self.pushScope();
    defer self.popScope();

    try self.writeBytes("{\n");

    self.indent_level += 1;
    for (block.items) |*statement| {
        try self.writeIndent();
        try self.compileStatement(statement);
    }
    self.indent_level -= 1;

    try self.writeIndent();
    try self.writeBytes("}\n\n");
}

fn compileTypeAst(self: *Self, t: ast.Type) CompilerError![]const u8 {
    return switch (t) {
        .symbol => |symbol| try self.compileType(try self.getSymbolType(symbol)),
        .reference => |reference| try std.fmt.allocPrint(self.alloc, "{s} *{s}", .{
            try self.compileTypeAst(reference.inner.*),
            if (reference.is_mut) "" else " const",
        }),
        else => |other| std.debug.panic("unimplemented type {s}\n", .{@tagName(other)}),
    };
}

fn compileType(self: *Self, t: Type) CompilerError![]const u8 {
    return switch (t) {
        .reference => |reference| try std.fmt.allocPrint(self.alloc, "{s} *{s}", .{
            try self.compileType(reference.inner.*),
            if (reference.is_mut) "" else " const",
        }),
        .@"struct" => |s| self.getInnerName(s.name),

        .i8 => "int8_t",
        .i16 => "int16_t",
        .i32 => "int32_t",
        .i64 => "int64_t",

        .u8 => "uint8_t",
        .u16 => "uint16_t",
        .u32 => "uint32_t",
        .u64 => "uint64_t",

        .f32 => "float",
        .f64 => "double",

        .void => "void",
        else => |other| std.debug.panic("unimplemented type: {any}\n", .{other}),
    };
}

/// Converts an AST type to a Compiler type.
/// `infer_expr` is the expression with which the type is inferred.
fn getTypeFromAst(
    self: *Self,
    t: union(enum) { strong: ast.Type, infer: ast.Expression },
) CompilerError!Type {
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

fn compileVariableSignature(self: *Self, name: []const u8, @"type": Type) CompilerError!void {
    switch (@"type") {
        .array => |array| {
            if (array.size) |size| {
                try self.write("{s} {s}[{}]", .{ try self.compileType(array.inner.*), name, size });
            } else std.debug.print("unimplemented arraylist\n", .{});
        },
        else => {
            try self.write("{s} ", .{try self.compileType(@"type")});
            try self.write("{s}", .{name});
        },
    }
}

fn compileVariableDefinition(self: *Self, v: ast.Statement.VariableDefinition) CompilerError!void {
    const variable_type = try self.getTypeFromAst(
        if (v.type == .inferred)
            .{ .infer = v.assigned_value }
        else
            .{ .strong = v.type },
    );

    if (!v.is_mut) try self.writeBytes("const ");

    try self.compileVariableSignature(v.variable_name, variable_type);

    try self.writeBytes(" = ");

    try self.compileExpression(&v.assigned_value);

    try self.writeBytes(";\n");

    try self.registerSymbol(v.variable_name, variable_type, .symbol);
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
            try self.write(" {s} ", .{switch (assignment.op) {
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
            try self.compileExpression(assignment.value);
        },
        .block => |block| try self.compileBlock(block),
        .binary => |binary| {
            try self.compileExpression(binary.lhs);
            try self.write(" {s} ", .{switch (binary.op) {
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
                .bang => "!",
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
            try self.writeBytes(if (try self.inferType(member.lhs.*) == .reference) "->" else ".");
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
        .range => std.debug.print("illegal range expression\n", .{}),
        .reference => |reference| {
            try self.writeBytes("&");
            try self.compileExpression(reference.inner);
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
