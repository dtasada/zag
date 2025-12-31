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
    SymbolNotVariable,
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
    path: []const u8,

    fn init(alloc: std.mem.Allocator, path: []const u8, file: std.fs.File) !File {
        var self: File = .{
            .handler = file,
            .path = path,
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
    symbol: struct {
        type: Type,
        is_mut: bool,
        inner_name: []const u8,
    },
    type: struct {
        type: Type,
        inner_name: []const u8,
    },
});

pub fn init(alloc: std.mem.Allocator, parser: *const Parser, file_path: []const u8) !*Self {
    const self = try alloc.create(Self);

    // mkdir .zag-out/
    var @".zag-out" = try std.fs.cwd().makeOpenPath(".zag-out", .{});
    defer @".zag-out".close();
    // mkdir .zag-out/src
    try @".zag-out".makePath(std.fs.path.dirname(file_path) orelse "");

    // mkdir .zag-out/zag
    var @".zag-out/zag" = try @".zag-out".makeOpenPath("zag", .{});
    defer @".zag-out/zag".close();

    const @".c" = try std.fmt.allocPrint(alloc, "{s}.c", .{file_path}); // src/main.zag -> src/main.zag.c
    const output_file = try @".zag-out".createFile(@".c", .{}); // mkdir .zag-out/src/main.zag.c

    const zag_header_path = try std.fs.path.join(alloc, &.{ ".zag-out", "zag", "zag.h" });
    const zag_header_file = try @".zag-out/zag".createFile("zag.h", .{});
    _ = try zag_header_file.write(
        \\#ifndef ZAG_H
        \\#define ZAG_H
        \\
        \\#include <stdbool.h>
        \\#include <stdint.h>
        \\
        \\typedef int8_t  i8;
        \\typedef int16_t i16;
        \\typedef int32_t i32;
        \\typedef int64_t i64;
        \\
        \\typedef uint8_t  u8;
        \\typedef uint16_t u16;
        \\typedef uint32_t u32;
        \\typedef uint64_t u64;
        \\
        \\typedef float  f32;
        \\typedef double f64;
        \\
        \\#define __ZAG_OPTIONAL_TYPE(union_name, inner_type) \
        \\    typedef struct {                                \
        \\        bool       is_some;                         \
        \\        inner_type payload;                         \
        \\    } union_name;
        \\
        \\#define __ZAG_ERROR_UNION_TYPE(union_name, error_type, success_type) \
        \\    typedef struct {                                                 \
        \\        bool is_success;                                             \
        \\        union {                                                      \
        \\            success_type success;                                    \
        \\            error_type   error;                                      \
        \\        } payload;                                                   \
        \\    } union_name;
        \\
        \\#endif
    );

    self.* = .{
        .alloc = alloc,
        .parser = parser,

        .output = try .init(alloc, @".c", output_file),
        .zag_header = try .init(alloc, zag_header_path, zag_header_file),
    };

    var @".zag-out/bin" = try @".zag-out".makeOpenPath("bin", .{});
    defer @".zag-out/bin".close();

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
        try statements.compile(self, &file_writer, statement);

    try self.output.write(file_writer.items);
    try self.output.flush();

    const out_c_file_path = try std.fs.path.join(self.alloc, &.{ ".zag-out", self.output.path }); // .zag-out/src/main.zag.c
    defer self.alloc.free(out_c_file_path);

    const main_obj = try std.fs.path.join(self.alloc, &.{ ".zag-out", "bin", "main" });
    const @"-Iinclude" = try std.fs.path.join(self.alloc, &.{ "-I./", ".zag-out", "zag" });

    std.debug.print("{s} {s} {s} {s} {s}\n", .{
        "/usr/bin/cc",
        "-o",
        main_obj,
        out_c_file_path,
        @"-Iinclude",
    });

    var cc = std.process.Child.init(&.{
        "/usr/bin/cc",
        "-o",
        main_obj,
        out_c_file_path,
        @"-Iinclude",
    }, self.alloc);

    cc.stdin_behavior = .Ignore;
    cc.stdout_behavior = .Pipe;
    cc.stderr_behavior = .Pipe;
    cc.spawn() catch |err| {
        utils.print("Couldn't spawn compiler command: {}\n", .{err}, .red);
        return;
    };
    const stdout = cc.stdout.?.readToEndAlloc(self.alloc, 1 << 20) catch return;
    const stderr = cc.stderr.?.readToEndAlloc(self.alloc, 1 << 20) catch return;

    const term = cc.wait() catch return;

    std.debug.print("exit: {}\n", .{term});
    std.debug.print("stdout:\n{s}\n", .{stdout});
    std.debug.print("stderr:\n{s}\n", .{stderr});
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
        try statements.compile(self, file_writer, statement);
    }
    self.indent_level -= 1;

    try self.indent(file_writer);
    try self.write(file_writer, "}\n\n");
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
        .function => |function| {
            try self.compileType(file_writer, function.return_type.*);
            try self.print(file_writer, " (*{s})(", .{name});
            for (function.params.items, 1..) |param, i| {
                try self.compileType(file_writer, param.*);
                if (i < function.params.items.len) try self.write(file_writer, ", ");
            }
            try self.write(file_writer, ")");
        },
        else => {
            try self.compileType(file_writer, @"type");
            try self.print(file_writer, " {s}", .{name});
        },
    }
}

pub fn solveComptimeExpression(self: *Self, expression: ast.Expression) !Value {
    return switch (expression) {
        .int => |int| .{ .i64 = int },
        .uint => |uint| .{ .u64 = uint },
        .float => |float| .{ .f64 = float },
        .char => |char| .{ .u8 = char },
        .binary => |binary| try (try self.solveComptimeExpression(binary.lhs.*))
            .binaryOperation(binary.op, try self.solveComptimeExpression(binary.rhs.*)),
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
    symbol_or_type: union(enum) {
        symbol: struct { is_mut: bool = false },
        type: void,
    },
) !void {
    var last = &self.scopes.items[self.scopes.items.len - 1];
    try last.put(name, switch (symbol_or_type) {
        .symbol => |symbol| .{
            .symbol = .{
                .is_mut = symbol.is_mut,
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

pub fn getSymbolMutability(self: *const Self, symbol: []const u8) !bool {
    var it = std.mem.reverseIterator(self.scopes.items);
    while (it.next()) |scope|
        return switch (scope.get(symbol) orelse continue) {
            .symbol => |s| s.is_mut,
            .type => return utils.printErr(
                error.SymbolNotVariable,
                "Compiler error: Symbol {s} is a type, not a variable\n",
                .{symbol},
                .red,
            ),
        };

    return utils.printErr(
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
