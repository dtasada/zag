const std = @import("std");

const utils = @import("utils");
const ast = @import("Parser").ast;

const expressions = @import("expressions.zig");
const statements = @import("statements.zig");

const Parser = @import("Parser");

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
    std.debug.assert(self.scopes.items.len == 0); // there should be no more scopes left

    for (self.scopes.items) |*scope| scope.deinit();
    self.scopes.deinit(self.alloc);
}

pub fn print(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    comptime fmt: []const u8,
    args: anytype,
) CompilerError!void {
    try file_writer.print(self.alloc, fmt, args);
}

pub fn write(self: *Self, file_writer: *std.ArrayList(u8), bytes: []const u8) CompilerError!void {
    try file_writer.appendSlice(self.alloc, bytes);
}

/// prints 4 spaces for each indent level into an arraylist
pub inline fn indent(self: *Self, file: *std.ArrayList(u8)) CompilerError!void {
    for (0..self.indent_level) |_|
        try file.appendSlice(self.alloc, "    ");
}

/// Entry point for the compiler. Compiles AST into C code.
pub fn emit(self: *Self) CompilerError!void {
    var file_writer: std.ArrayList(u8) = .empty;

    try self.write(&file_writer, "#include <zag.h>\n");

    for (self.parser.output.items) |*statement|
        try statements.compileStatement(self, &file_writer, statement);

    try self.output.write(file_writer.items);
    try self.output.flush();
}

pub fn compileFunctionDefinition(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    function_def: ast.Statement.FunctionDefinition,
) CompilerError!void {
    try self.registerSymbol(function_def.name, try .fromAst(self, .{ .strong = function_def.getType() }), .symbol);

    try self.compileTypeAst(file_writer, function_def.return_type);
    try self.print(file_writer, " {s}(", .{function_def.name});
    for (function_def.parameters.items, 1..) |parameter, i| {
        try self.compileVariableSignature(file_writer, parameter.name, try .fromAst(self, .{ .strong = parameter.type }));
        if (i < function_def.parameters.items.len) try self.write(file_writer, ", ");
    }
    try self.write(file_writer, ") ");

    try self.compileBlock(file_writer, function_def.body);
}

pub fn compileBlock(
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
        try statements.compileStatement(self, file_writer, statement);
    }
    self.indent_level -= 1;

    try self.indent(file_writer);
    try self.write(file_writer, "}\n\n");
}

pub fn compileTypeAst(self: *Self, file_writer: *std.ArrayList(u8), t: ast.Type) CompilerError!void {
    switch (t) {
        .symbol => |symbol| try self.compileType(file_writer, try self.getSymbolType(symbol)),
        .reference => |reference| {
            try self.compileTypeAst(file_writer, reference.inner.*);
            try self.print(file_writer, " *{s}", .{if (reference.is_mut) "" else " const"});
        },
        else => |other| std.debug.panic("unimplemented type {s}\n", .{@tagName(other)}),
    }
}

pub fn compileType(self: *Self, file_writer: *std.ArrayList(u8), t: Type) CompilerError!void {
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

pub fn compileVariableSignature(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    name: []const u8,
    @"type": Type,
) CompilerError!void {
    switch (@"type") {
        .array => |array| if (array.size) |size| {
            try self.compileType(file_writer, array.inner.*);
            try self.print(file_writer, " {s}[{}]", .{ name, size });
        } else std.debug.print("unimplemented arraylist\n", .{}),
        else => {
            try self.compileType(file_writer, @"type");
            try self.print(file_writer, " {s}", .{name});
        },
    }
}

pub fn solveComptimeExpression(self: *Self, expression: ast.Expression) !Value {
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
pub fn pushScope(self: *Self) !void {
    try self.scopes.append(self.alloc, .init(self.alloc));
}

/// pops the scope of the scope stack.
pub fn popScope(self: *Self) void {
    var last = self.scopes.pop().?;
    last.deinit();
}

/// registers a new entry in the top scope of the scope stack.
pub fn registerSymbol(
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

pub fn getSymbolType(self: *const Self, symbol: []const u8) !Type {
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
