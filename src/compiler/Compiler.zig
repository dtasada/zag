const std = @import("std");

const utils = @import("utils");
const ast = @import("Parser").ast;

const expressions = @import("expressions.zig");
const statements = @import("statements.zig");
const errors = @import("errors.zig");

const Lexer = @import("Lexer");
const Parser = @import("Parser");

const Type = @import("Type.zig").Type;
const Value = @import("Value.zig").Value;

const Self = @This();

pub const Module = @import("Module.zig");

pub const CompilerError = error{
    AssignmentToImmutableVariable,
    BadMutability,
    DuplicateMember,
    FailedToCreateParser,
    FailedToReadSource,
    FailedToTokenizeSource,
    IllegalExpression,
    IllegalStatement,
    MemberExpressionOnPrimitiveType,
    MemberIsNotAMethod,
    MissingArguments,
    MissingElseClause,
    NoSuchMember,
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
} || Parser.ParserError ||
    Lexer.LexerError ||
    std.fs.Dir.MakeError ||
    std.fs.Dir.OpenError ||
    std.fs.Dir.StatFileError ||
    std.fs.File.OpenError ||
    std.Io.Writer.Error;

pub const Mode = enum { emit, analysis };

alloc: std.mem.Allocator,

/// Input AST to be compiled.
input: ast.RootNode,
source_path: []const u8,

module_registry: *std.StringHashMap(Module),
exported_symbols: std.StringHashMap(Module.Symbol),

/// Points to whichever file should be written to.
writer: ?*File,

/// The C source file being written to.
output: ?File,

/// The helper `zag.h` header file that contains zag language level dependencies like auto-generated types.
zag_header: ?File,

/// The helper `zag.c` source file that contains zag language level implementations like auto-generated types.
zag_source: ?File,

/// The header file for the module being compiled (e.g. `lib.zag.h`).
module_header: ?File,

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
pub const Scope = std.StringHashMap(ScopeItem);
pub const ScopeItem = union(enum) {
    symbol: struct {
        type: Type,
        is_mut: bool,
        inner_name: []const u8,
    },
    type: struct {
        type: Type,
        inner_name: []const u8,
    },
    module: Module,
};

pub fn getAST(
    alloc: std.mem.Allocator,
    file_path: []const u8,
) CompilerError!struct {
    root: ast.RootNode,
    source: []u8,
} {
    const file = std.fs.cwd().readFileAlloc(alloc, file_path, 1 << 20) catch |err|
        return utils.printErr(
            error.FailedToReadSource,
            "Failed to open file '{s}': {}\n",
            .{ file_path, err },
            .red,
        );

    var lexer = Lexer.init(file, alloc, file_path) catch |err|
        return utils.printErr(
            error.FailedToTokenizeSource,
            "Failed to tokenize source code: {}\n",
            .{err},
            .red,
        );
    defer lexer.deinit(alloc);

    var parser = Parser.init(lexer, alloc) catch |err|
        return utils.printErr(
            error.FailedToCreateParser,
            "Failed to create parser: {}\n",
            .{err},
            .red,
        );
    defer parser.deinit();

    return .{
        .root = try parser.output.clone(alloc),
        .source = file,
    };
}

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
pub fn init(
    alloc: std.mem.Allocator,
    input: ast.RootNode,
    file_path: []const u8,
    registry: *std.StringHashMap(Module),
    mode: Mode,
) CompilerError!*Self {
    const self = try alloc.create(Self);

    var visited: std.ArrayList(Type.Context.Visited) = try .initCapacity(alloc, 128);

    self.* = .{
        .alloc = alloc,
        .input = input,
        .source_path = try alloc.dupe(u8, file_path),
        .module_registry = registry,
        .exported_symbols = .init(alloc),
        .output = null,
        .module_header = null,
        .zag_header = null,
        .zag_source = null,
        .zag_header_contents = .initContext(alloc, .{ .visited = &visited }),
        .writer = null,
    };

    switch (mode) {
        .emit => {
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

            const @".h" = try std.fmt.allocPrint(alloc, "{s}.h", .{file_path});
            const module_header_file = try @".zag-out".createFile(@".h", .{});

            const zag_header_path = try std.fs.path.join(alloc, &.{ ".zag-out", "zag", "zag.h" });
            const zag_header_file = try @".zag-out/zag".createFile("zag.h", .{});

            const zag_source_path = try std.fs.path.join(alloc, &.{ ".zag-out", "zag", "zag.c" });
            const zag_source_file = try @".zag-out/zag".createFile("zag.c", .{});

            self.output = try .init(alloc, @".c", output_file);
            self.module_header = try .init(alloc, @".h", module_header_file);
            self.zag_header = try .init(alloc, zag_header_path, zag_header_file);
            self.zag_source = try .init(alloc, zag_source_path, zag_source_file);

            self.writer = &self.output.?;

            try self.zag_source.?.write(
                \\#include <stdlib.h>
                \\#include <zag.h>
                \\
            );

            try self.zag_header.?.write(@import("zag.h.zig").CONTENT);

            // Header Guard
            const guard_name = try std.fmt.allocPrint(alloc, "__ZAG_MODULE_{}_H", .{std.hash.Wyhash.hash(0, file_path)});
            try self.module_header.?.print("#ifndef {s}\n#define {s}\n\n#include <zag.h>\n\n", .{ guard_name, guard_name });

            // Include module header in module source
            try self.output.?.print("#include \"{s}.h\"\n\n", .{std.fs.path.basename(file_path)});

            var @".zag-out/bin" = try @".zag-out".makeOpenPath("bin", .{});
            defer @".zag-out/bin".close();
        },
        .analysis => {},
    }

    return self;
}

/// Frees resources
pub fn deinit(self: *Self) void {
    self.alloc.free(self.source_path);
    if (self.output) |*o| o.deinit(self.alloc);
    if (self.module_header) |*h| h.deinit(self.alloc);
    if (self.zag_header) |*z| z.deinit(self.alloc);
    self.scopes.deinit(self.alloc);
}

/// Prints formatted content to whichever file handle `self.writer` is pointing to at the moment.
pub fn print(
    self: *Self,
    comptime fmt: []const u8,
    args: anytype,
) CompilerError!void {
    const w = self.writer orelse @panic("Compiler attempted to write in analysis mode");
    try w.print(fmt, args);
}

/// Writes bytes to whichever file handle `self.writer` is pointing to at the moment.
pub fn write(self: *Self, bytes: []const u8) CompilerError!void {
    const w = self.writer orelse @panic("Compiler attempted to write in analysis mode");
    try w.write(bytes);
}

/// Flushes whichever file handle `self.writer` is pointing to at the moment
pub fn flush(self: *Self) CompilerError!void {
    const w = self.writer orelse @panic("Compiler attempted to write in analysis mode");
    try w.flush();
}

/// Prints 4 spaces for each indent level into whichever file handle `self.writer` is pointing to
/// at the moment.
pub inline fn indent(self: *Self) CompilerError!void {
    const w = self.writer orelse @panic("Compiler attempted to write in analysis mode");
    for (0..self.indent_level) |_|
        try w.write("    ");
}

/// Entry point for the compiler. Compiles AST into C code.
pub fn emit(self: *Self) CompilerError!void {
    try self.pushScope();
    defer self.popScope();

    // register constants
    try self.registerConstants();

    try self.write("#include <zag.h>\n");

    for (self.input.items) |*statement|
        try statements.compile(self, statement);

    try self.output.?.flush();

    try self.zag_header.?.write("\n#endif");
    try self.zag_header.?.flush();

    try self.module_header.?.write("\n#endif");
    try self.module_header.?.flush();
}

pub fn analyze(self: *Self) CompilerError!void {
    try self.pushScope();
    defer self.popScope();

    try self.registerConstants();

    for (self.input.items) |*statement| {
        switch (statement.*) {
            .import => |*import_stmt| {
                _ = try self.processImport(import_stmt);
            },
            .function_definition => |*func| {
                const t = try Type.fromAst(self, func.getType());
                try self.registerSymbol(func.name, .{ .symbol = .{ .type = t } });
                if (func.is_pub) {
                    try self.exported_symbols.put(func.name, .{
                        .name = func.name,
                        .is_pub = true,
                        .type = t,
                    });
                }
            },
            .binding_function_declaration => |*func| {
                const t = try Type.fromAst(self, func.getType());
                try self.registerSymbol(func.name, .{ .symbol = .{ .type = t } });
                if (func.is_pub) {
                    try self.exported_symbols.put(func.name, .{
                        .name = func.name,
                        .is_pub = true,
                        .type = t,
                    });
                }
            },
            .variable_definition => |*var_def| {
                // for variables, we need to infer type if possible or take declared type
                // logic similar to variableDefinition in statements.zig
                const received_type: Type = try .infer(self, var_def.assigned_value);
                const expected_type: ?Type = if (var_def.type == .inferred) null else try .fromAst(self, var_def.type);

                const final_type = expected_type orelse received_type;

                try self.registerSymbol(var_def.variable_name, .{
                    .symbol = .{
                        .is_mut = var_def.is_mut,
                        .type = final_type,
                    },
                });

                if (var_def.is_pub) {
                    try self.exported_symbols.put(var_def.variable_name, .{
                        .name = var_def.variable_name,
                        .is_pub = true,
                        .type = final_type,
                    });
                }
            },
            inline .struct_declaration, .union_declaration, .enum_declaration => |struct_decl| {
                var compound_type: switch (@TypeOf(struct_decl)) {
                    ast.Statement.StructDeclaration => Type.Struct,
                    ast.Statement.UnionDeclaration => Type.Union,
                    ast.Statement.EnumDeclaration => Type.Enum,
                    else => unreachable,
                } = try .init(self.alloc, struct_decl.name, null);

                const symbol_type = @unionInit(Type, switch (@TypeOf(struct_decl)) {
                    ast.Statement.StructDeclaration => "struct",
                    ast.Statement.UnionDeclaration => "union",
                    ast.Statement.EnumDeclaration => "enum",
                    else => unreachable,
                }, compound_type);

                try self.registerSymbol(struct_decl.name, .{ .type = symbol_type });

                // Populate members
                switch (@TypeOf(struct_decl)) {
                    ast.Statement.StructDeclaration => for (struct_decl.members.items) |member| {
                        try compound_type.members.put(member.name, try .fromAstPtr(self, member.type));
                    },
                    ast.Statement.UnionDeclaration => for (struct_decl.members.items) |member| {
                        try compound_type.members.put(member.name, b: {
                            const member_type = try self.alloc.create(Type);
                            member_type.* = if (member.type) |t| try .fromAst(self, t) else .void;
                            break :b member_type;
                        });
                    },
                    ast.Statement.EnumDeclaration => {
                        var enum_last_value: usize = 0;
                        for (struct_decl.members.items) |member| {
                            try compound_type.members.put(member.name, if (member.value) |value| b: {
                                enum_last_value = (try self.solveComptimeExpression(value)).u64;
                                break :b enum_last_value;
                            } else b: {
                                const val = enum_last_value + 1;
                                enum_last_value += 1;
                                break :b val;
                            });
                        }
                    },
                    else => unreachable,
                }

                // Populate methods
                for (struct_decl.methods.items) |method| {
                    var params: std.ArrayList(Type.Function.Param) = try .initCapacity(
                        self.alloc,
                        method.parameters.items.len,
                    );
                    for (method.parameters.items) |p|
                        params.appendAssumeCapacity(.{
                            .name = p.name,
                            .type = try .fromAst(self, p.type),
                        });
                    const return_type: *Type = try .fromAstPtr(self, method.return_type);
                    try compound_type.methods.put(method.name, .{
                        .inner_name = try std.fmt.allocPrint(self.alloc, "__zag_{s}_{s}", .{
                            struct_decl.name,
                            method.name,
                        }),
                        .params = params,
                        .return_type = return_type,
                    });
                }

                if (struct_decl.is_pub) {
                    try self.exported_symbols.put(struct_decl.name, .{
                        .name = struct_decl.name,
                        .is_pub = true,
                        .type = symbol_type,
                    });
                }
            },
            else => {},
        }
    }
}

pub fn processImport(self: *Self, import_stmt: *const ast.Statement.Import) CompilerError!Module {
    var parts: std.ArrayList([]const u8) = .empty;
    defer parts.deinit(self.alloc);

    // Add directory of current file
    if (std.fs.path.dirname(self.source_path)) |dir| try parts.append(self.alloc, dir);
    for (import_stmt.module_name.items) |part| try parts.append(self.alloc, part);

    const rel_path = try std.fs.path.join(self.alloc, parts.items);
    defer self.alloc.free(rel_path);

    const full_path = try std.fmt.allocPrint(self.alloc, "{s}.zag", .{rel_path});
    defer self.alloc.free(full_path);

    // Check registry
    if (self.module_registry.get(full_path)) |mod| {
        return mod;
    }

    // Analyze new module
    const ast_res = try getAST(self.alloc, full_path); // recursive AST

    // We pass the SAME registry
    var child_compiler = try init(self.alloc, ast_res.root, full_path, self.module_registry, .analysis);
    defer child_compiler.deinit();

    try child_compiler.analyze();

    // Create module from exports
    var mod = Module.init(self.alloc, import_stmt.alias orelse import_stmt.module_name.getLast());
    mod.source_buffer = ast_res.source;

    var it = child_compiler.exported_symbols.iterator();
    while (it.next()) |entry| {
        try mod.symbols.put(entry.key_ptr.*, entry.value_ptr.*);
    }

    // Register
    // We need to duplicate the path key because it will be freed
    const key = try self.alloc.dupe(u8, full_path);
    try self.module_registry.put(key, mod);

    return mod;
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
    try self.write("}\n");
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
        .@"union" => |u| try self.write(try self.getInnerName(u.name)),
        .@"enum" => |e| try self.write(try self.getInnerName(e.name)),
        .optional => |optional| {
            const type_name = try std.fmt.allocPrint(self.alloc, "__zag_Optional_{}", .{t.hash()});
            if (self.zag_header_contents.get(t) == null) {
                self.writer = &self.zag_header.?;
                defer self.writer = &self.output.?;

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
                self.writer = &self.zag_header.?;
                defer self.writer = &self.output.?;

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
                self.writer = &self.zag_header.?;

                // write type definition to zag.h
                try self.print("__ZAG_ARRAYLIST_DEF({s}, ", .{type_name});
                try self.compileType(arraylist.*, new_opts);
                try self.write(")\n");

                self.writer = &self.zag_source.?;

                // write type implementation to zag.c
                try self.print("__ZAG_ARRAYLIST_IMPL({s}, ", .{type_name});
                try self.compileType(arraylist.*, new_opts);
                try self.write(")\n");
                try self.flush();

                self.writer = &self.output.?;

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
                try self.compileType(param.type, .{ .binding_mut = opts.binding_mut });
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
    symbol_or_type: union(enum) {
        symbol: struct { is_mut: bool = false, type: Type },
        type: Type,
        module: Module,
    },
) !void {
    var last = &self.scopes.items[self.scopes.items.len - 1];
    try last.put(name, switch (symbol_or_type) {
        .symbol => |symbol| .{
            .symbol = .{
                .is_mut = symbol.is_mut,
                .type = symbol.type,
                // UNIMPLEMENTED: Name mangling is not implemented.
                // Symbols are emitted with their raw names, which will cause link errors if multiple modules define the same symbol.
                .inner_name = name,
            },
        },
        .type => |@"type"| .{
            .type = .{
                .type = @"type",
                // UNIMPLEMENTED: Name mangling is not implemented.
                .inner_name = name,
            },
        },
        .module => |module| .{ .module = module },
    });
}

/// Registers boolean constants, null, undefined, and primitive types
fn registerConstants(self: *Self) !void {
    try self.registerSymbol("true", .{ .symbol = .{ .type = .bool } });
    try self.registerSymbol("false", .{ .symbol = .{ .type = .bool } });
    try self.registerSymbol("null", .{ .symbol = .{ .type = .@"typeof(null)" } });
    try self.registerSymbol("undefined", .{ .symbol = .{ .type = .@"typeof(undefined)" } });

    try self.registerSymbol("i8", .{ .type = .i8 });
    try self.registerSymbol("i16", .{ .type = .i16 });
    try self.registerSymbol("i32", .{ .type = .i32 });
    try self.registerSymbol("i64", .{ .type = .i64 });

    try self.registerSymbol("u8", .{ .type = .u8 });
    try self.registerSymbol("u16", .{ .type = .u16 });
    try self.registerSymbol("u32", .{ .type = .u32 });
    try self.registerSymbol("u64", .{ .type = .u64 });

    try self.registerSymbol("usize", .{ .type = .usize });

    try self.registerSymbol("f32", .{ .type = .f32 });
    try self.registerSymbol("f64", .{ .type = .f64 });

    try self.registerSymbol("void", .{ .type = .void });
    try self.registerSymbol("bool", .{ .type = .bool });

    try self.registerSymbol("c_int", .{ .type = .c_int });
    try self.registerSymbol("c_char", .{ .type = .c_char });
}

pub fn getSymbolType(self: *const Self, symbol: []const u8) !Type {
    var it = std.mem.reverseIterator(self.scopes.items);
    while (it.next()) |scope|
        return switch (scope.get(symbol) orelse continue) {
            .module => |module| .{ .module = module },
            inline .symbol, .type => |s| s.type,
        };

    // return primitive type
    return error.UnknownSymbol;
}

pub fn getScopeItem(self: *const Self, symbol: []const u8) !ScopeItem {
    var it = std.mem.reverseIterator(self.scopes.items);
    while (it.next()) |scope| return scope.get(symbol) orelse continue;

    return error.SymbolNotVariable;
}

pub fn getSymbolMutability(self: *const Self, symbol: []const u8) !bool {
    var it = std.mem.reverseIterator(self.scopes.items);
    while (it.next()) |scope|
        return switch (scope.get(symbol) orelse continue) {
            .symbol => |s| s.is_mut,
            else => return utils.printErr(
                error.SymbolNotVariable,
                "Compiler error: Symbol {s} is not a variable\n",
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
    while (it.next()) |scope| {
        if (scope.get(symbol)) |item| {
            return switch (item) {
                .module => |module| module.name,
                inline else => |s| s.inner_name,
            };
        }
    }

    // Search in modules in scope
    it = std.mem.reverseIterator(self.scopes.items);
    while (it.next()) |scope| {
        var scope_it = scope.iterator();
        while (scope_it.next()) |entry| {
            switch (entry.value_ptr.*) {
                .module => |module| {
                    if (module.symbols.get(symbol)) |s| {
                        // TODO: mangling
                        return s.name;
                    }
                },
                else => {},
            }
        }
    }

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
