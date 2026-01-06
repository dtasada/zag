const std = @import("std");

const utils = @import("utils");
const ast = @import("Parser").ast;

const expressions = @import("expressions.zig");
const statements = @import("statements.zig");
const errors = @import("errors.zig");

const Parser = @import("Parser");

const Type = @import("Type.zig").Type;
const Value = @import("Value.zig").Value;

const Self = @This();

pub const CompilerError = error{
    AssignmentToImmutableVariable,
    BadMutability,
    DuplicateMember,
    IllegalExpression,
    IllegalStatement,
    MemberExpressionOnPrimitiveType,
    MemberIsNotAMethod,
    MissingArguments,
    MissingElseClause,
    OutOfMemory,
    SymbolNotVariable,
    TooManyArguments,
    TypeMismatch,
    TypeNotPrimitive,
    UndeclaredField,
    UndeclaredProperty,
    UndeclaredType,
    UndeclaredVariable,
    UnknownSymbol,
    UnsupportedExpression,
    UnsupportedType,
    VariableRedeclaration,
} || Parser.ParserError || std.Io.Writer.Error;

alloc: std.mem.Allocator,

/// Input AST to be compiled.
input: []const ast.Statement,

/// Points to whichever file should be written to.
writer: *File,

/// The C source file being written to.
output: File,

/// The helper `zag.h` header file that contains zag language level dependencies like auto-generated types.
zag_header: File,

/// The helper `zag.c` source file that contains zag language level implementations like auto-generated types.
zag_source: File,

/// Maps a type to its inner name.
zag_header_contents: std.HashMap(
    Type,
    []const u8,
    Type.Context,
    std.hash_map.default_max_load_percentage,
),

/// Keeps track of how many spaces should be printed when formatting code.
indent_level: usize = 0,

/// Stack of scopes.
scopes: std.ArrayList(Scope) = .empty,

/// Maps a symbol name to the symbol's type and inner name.
const ScopeItem = union(enum) {
    symbol: struct {
        type: Type,
        is_mut: bool,
        inner_name: []const u8,
    },
    type: struct {
        type: Type,
        inner_name: []const u8,
    },
};
const Scope = std.StringHashMap(ScopeItem);

/// Helper file handling structure. Used for commodity when writing to a file.
const File = struct {
    handler: std.fs.File,
    writer: std.fs.File.Writer,
    buf: []u8,
    path: []const u8,

    /// creates a new file handler.
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

    /// flushes the file writer and closes the file handle
    fn deinit(self: *File, alloc: std.mem.Allocator) void {
        self.flush() catch |err| utils.print(
            "Compiler error: failed to write to file: {}\n",
            .{err},
            .red,
        );
        self.handler.close();
        alloc.free(self.buf);
    }

    /// writes `bytes` to the file writer
    fn write(self: *File, bytes: []const u8) !void {
        _ = try self.writer.interface.write(bytes);
    }

    /// prints a formatted string to the file writer.
    fn print(self: *File, comptime fmt: []const u8, args: anytype) CompilerError!void {
        try self.writer.interface.print(fmt, args);
    }

    /// flushes the filewriter
    fn flush(self: *File) !void {
        try self.writer.interface.flush();
    }
};

/// initializes a new `Compiler`.
/// creates `.zag-out` folder and mirrors the `src` directory with compiled C code.
/// also creates `.zag-out/bin` folder but doesn't write anything to it.
pub fn init(alloc: std.mem.Allocator, input: []const ast.Statement, file_path: []const u8) !*Self {
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

    const zag_source_path = try std.fs.path.join(alloc, &.{ ".zag-out", "zag", "zag.c" });
    const zag_source_file = try @".zag-out/zag".createFile("zag.c", .{});

    var visited: std.ArrayList(Type.Context.Visited) = try .initCapacity(alloc, 128);
    self.* = .{
        .alloc = alloc,
        .input = input,

        .output = try .init(alloc, @".c", output_file),
        .zag_header = try .init(alloc, zag_header_path, zag_header_file),
        .zag_source = try .init(alloc, zag_source_path, zag_source_file),
        .zag_header_contents = .initContext(alloc, .{ .visited = &visited }), // TODO: will probably change in the future, when compiling files
        .writer = undefined,
    };
    self.writer = &self.output;

    try self.zag_source.write(
        \\#include <stdlib.h>
        \\#include <zag.h>
        \\
    );

    try self.zag_header.write(@import("zag.h.zig").CONTENT);

    var @".zag-out/bin" = try @".zag-out".makeOpenPath("bin", .{});
    defer @".zag-out/bin".close();

    return self;
}

/// Frees resources
pub fn deinit(self: *Self) void {
    self.output.deinit(self.alloc);
    self.zag_header.deinit(self.alloc);
    self.scopes.deinit(self.alloc);
}

/// Prints formatted content to whichever file handle `self.writer` is pointing to at the moment.
pub fn print(
    self: *Self,
    comptime fmt: []const u8,
    args: anytype,
) CompilerError!void {
    try self.writer.print(fmt, args);
}

/// Writes bytes to whichever file handle `self.writer` is pointing to at the moment.
pub fn write(self: *Self, bytes: []const u8) CompilerError!void {
    try self.writer.write(bytes);
}

/// Flushes whichever file handle `self.writer` is pointing to at the moment
pub fn flush(self: *Self) CompilerError!void {
    try self.writer.flush();
}

/// Prints 4 spaces for each indent level into whichever file handle `self.writer` is pointing to
/// at the moment.
pub inline fn indent(self: *Self) CompilerError!void {
    for (0..self.indent_level) |_|
        try self.writer.write("    ");
}

/// Entry point for the compiler. Compiles AST into C code.
pub fn emit(self: *Self) CompilerError!void {
    try self.pushScope();
    defer self.popScope();

    // register constants
    try self.registerSymbol("true", .bool, .{ .symbol = .{} });
    try self.registerSymbol("false", .bool, .{ .symbol = .{} });
    try self.registerSymbol("null", .@"typeof(null)", .{ .symbol = .{} });
    try self.registerSymbol("undefined", .@"typeof(undefined)", .{ .symbol = .{} });

    try self.write("#include <zag.h>\n");

    for (self.input) |*statement|
        try statements.compile(self, statement);

    try self.output.flush();

    try self.zag_header.write("\n#endif");
    try self.zag_header.flush();

    const out_c_file_path = try std.fs.path.join(self.alloc, &.{ ".zag-out", self.output.path }); // .zag-out/src/main.zag.c
    defer self.alloc.free(out_c_file_path);

    const main_obj = try std.fs.path.join(self.alloc, &.{ ".zag-out", "bin", "main" });
    const @"-Iinclude" = try std.fs.path.join(self.alloc, &.{ "-I./", ".zag-out", "zag" });

    const cmd_args: []const []const u8 = &.{
        "/usr/bin/cc",
        "-o",
        main_obj,
        out_c_file_path,
        @"-Iinclude",
        "-Wall",
        "-Wextra",
    };
    for (cmd_args) |arg| std.debug.print("{s} \n", .{arg});
    std.debug.print("\n", .{});
    var cc = std.process.Child.init(cmd_args, self.alloc);

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
    block: ast.Block,
    opts: struct {
        capture: ?struct {
            condition: *const ast.Expression,
            name: []const u8,
        } = null,
        iterator: ?struct {
            iter_expr: *const ast.Expression,
            capture_name: []const u8,
            index: []const u8,
        } = null,
    },
) CompilerError!void {
    try self.pushScope();
    defer self.popScope();

    try self.write("{\n");

    self.indent_level += 1;

    if (opts.capture) |capture| {
        try self.indent();
        try self.compileVariableSignature(
            capture.name,
            (try Type.infer(self, capture.condition.*)).optional.*,
            .{},
        );
        try self.write(" = (");
        try expressions.compile(self, capture.condition, .{});
        try self.write(").payload;\n");
    }

    if (opts.iterator) |iterator| {
        try self.indent();
        try self.compileVariableSignature(
            iterator.capture_name,
            (try Type.infer(self, iterator.iter_expr.*)).array.inner.*,
            .{},
        );
        try self.write(" = (");
        try expressions.compile(self, iterator.iter_expr, .{});
        try self.print(").items[{s}];\n", .{iterator.index});
    }

    for (block.items) |*statement| {
        try self.indent();
        try statements.compile(self, statement);
    }
    self.indent_level -= 1;

    try self.indent();
    try self.write("}\n\n");
}

pub fn compileType(
    self: *Self,
    t: Type,
    opts: struct {
        binding_mut: bool = false,
        /// purely to prevent duplicate `const` qualifiers
        is_top_level: bool = true,
    },
) CompilerError!void {
    if (opts.is_top_level and !opts.binding_mut and t != .reference) try self.write("const ");
    const new_opts: @TypeOf(opts) = .{ .binding_mut = opts.binding_mut, .is_top_level = false };

    return switch (t) {
        .reference => |reference| {
            if (!reference.is_mut) try self.write("const ");

            try self.compileType(reference.inner.*, new_opts);
            try self.write(" *");

            if (!opts.binding_mut) try self.write(" const");
        },
        .@"struct" => |s| try self.write(try self.getInnerName(s.name)),
        .optional => |optional| {
            const type_name = try std.fmt.allocPrint(self.alloc, "__zag_Optional_{}", .{t.hash()});
            if (self.zag_header_contents.get(t) == null) {
                self.writer = &self.zag_header;
                defer self.writer = &self.output;

                try self.print("__ZAG_OPTIONAL_TYPE({s}, ", .{type_name});
                try self.compileType(optional.*, new_opts);
                try self.write(")\n");
                try self.flush();

                try self.zag_header_contents.put(t, type_name);
            }

            try self.write(type_name);
        },

        .error_union => |error_union| {
            const type_name = try std.fmt.allocPrint(self.alloc, "__zag_ErrorUnion_{}", .{t.hash()});
            if (self.zag_header_contents.get(t) == null) {
                self.writer = &self.zag_header;
                defer self.writer = &self.output;

                try self.print("__ZAG_ERROR_UNION_TYPE({s}, ", .{type_name});
                try self.compileType(error_union.failure.*, new_opts);
                try self.write(", ");
                try self.compileType(error_union.success.*, new_opts);
                try self.write(")\n");
                try self.flush();

                try self.zag_header_contents.put(t, type_name);
            }

            try self.write(type_name);
        },

        .array => |array| {
            try self.compileType(array.inner.*, new_opts);
            try self.print("[{}]", .{array.size});
        },
        .arraylist => |arraylist| {
            const type_name = try std.fmt.allocPrint(self.alloc, "__zag_ArrayList_{}", .{t.hash()});
            if (self.zag_header_contents.get(t) == null) {
                self.writer = &self.zag_header;

                // write type definition to zag.h
                try self.print("__ZAG_ARRAYLIST_DEF({s}, ", .{type_name});
                try self.compileType(arraylist.*, new_opts);
                try self.write(")\n");

                self.writer = &self.zag_source;

                // write type implementation to zag.c
                try self.print("__ZAG_ARRAYLIST_IMPL({s}, ", .{type_name});
                try self.compileType(arraylist.*, new_opts);
                try self.write(")\n");
                try self.flush();

                self.writer = &self.output;

                try self.zag_header_contents.put(t, type_name);
            }

            try self.write(type_name);
        },

        // should be unreachable, array and function types are handled in `compileVariableSignature`
        .function, .variadic => unreachable,
        else => |primitive| try self.write(@tagName(primitive)),
    };
}

pub fn compileVariableSignature(
    self: *Self,
    name: []const u8,
    t: Type,
    opts: struct {
        binding_mut: bool = false,
    },
) CompilerError!void {
    switch (t) {
        .array => |array| {
            try self.compileType(array.inner.*, .{ .binding_mut = opts.binding_mut });
            try self.print(" {s}[{}]", .{ name, array.size });
        },
        .function => |function| {
            try self.compileType(function.return_type.*, .{ .binding_mut = opts.binding_mut });
            try self.print(" (*{s})(", .{name});
            for (function.params.items, 1..) |param, i| {
                try self.compileType(param.*, .{ .binding_mut = opts.binding_mut });
                if (i < function.params.items.len) try self.write(", ");
            }
            try self.write(")");
        },
        .variadic => try self.write("..."),
        else => {
            try self.compileType(t, .{ .binding_mut = opts.binding_mut });
            try self.print(" {s}", .{name});
        },
    }
}

pub fn solveComptimeExpression(self: *Self, expression: ast.Expression) !Value {
    return switch (expression) {
        .int => |int| .{ .i64 = int.int },
        .uint => |uint| .{ .u64 = uint.uint },
        .float => |float| .{ .f64 = float.float },
        .char => |char| .{ .u8 = char.char },
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

    // return primitive type
    return try Type.fromSymbol(symbol);
}

pub fn getScopeItem(self: *const Self, symbol: []const u8) !ScopeItem {
    var it = std.mem.reverseIterator(self.scopes.items);
    while (it.next()) |scope| return scope.get(symbol) orelse continue;

    return if (Type.fromSymbol(symbol) catch null) |t| .{
        .type = .{
            .type = t,
            .inner_name = symbol,
        },
    } else error.SymbolNotVariable;
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
        "comperr: Unknown symbol: {s}\n",
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
        "comperr: Unknown symbol: {s}\n",
        .{symbol},
        .red,
    );
}

pub fn checkType(_: *const Self, expected_type: Type, received_type: Type) bool {
    return expected_type.eql(received_type) or received_type.convertsTo(expected_type);
}
