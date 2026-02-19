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
pub const Mode = enum { emit, analysis };

const CompilerError = errors.CompilerError;

const Section = struct {
    const Type = enum {
        // Header sections (for .h files)
        header_guard_start,
        header_includes,
        header_forward_decls,
        header_type_defs,
        header_function_decls,
        header_guard_end,

        // Source sections (for .c files)
        source_includes,
        source_helper_functions,
        source_type_impls,
        source_function_impls,

        zag_header_types,
        zag_header_macros,
    };

    pos: usize = 0,
    current_statement: usize = 0,
    buffer: std.ArrayList(u8) = .empty,
};

const Buffer = struct {
    writer: std.Io.Writer,

    fn init(alloc: std.mem.Allocator) !Buffer {
        return .{
            .writer = std.Io.Writer.Allocating.init(alloc),
        };
    }
};

alloc: std.mem.Allocator,

/// Input AST to be compiled.
input: ast.RootNode,
source_path: []const u8,

module: Module,
module_registry: *std.StringHashMap(Module),
exported_symbols: std.StringHashMap(Module.Symbol),

sections: std.EnumArray(Section.Type, Section),
current_section: Section.Type,
output_path: []const u8,
module_header_path: []const u8,
zag_header_path: []const u8,

/// Maps a type to its inner name.
zag_header_contents: std.HashMap(
    Type,
    []const u8,
    Type.Context,
    std.hash_map.default_max_load_percentage,
),

/// Stack of scopes.
scopes: std.ArrayList(Scope) = .empty,

current_return_type: ?Type = null,

/// Maps a pending generic type instantiation to its inner name.
pending_instantiations: std.ArrayList(GenericInstantiation),

imported_modules: std.StringHashMap(Module),

const GenericInstantiation = struct {
    inner_name: []const u8,
    args: []const Value,
    module: Module,
    t: GenericInstantiation.Type,

    pub const Type = union(enum) {
        @"struct": ast.Statement.StructDeclaration,
        @"union": ast.Statement.UnionDeclaration,
        function: ast.Statement.FunctionDefinition,
    };
};

/// Maps a symbol name to the symbol's type and inner name.
pub const Scope = struct {
    pending_defers: *std.ArrayList(*const ast.Statement),
    items: *std.StringHashMap(ScopeItem),
};
pub const ScopeItem = union(enum) {
    symbol: struct {
        type: Type,
        is_mut: bool,
        inner_name: []const u8,
        is_defined: bool,
    },
    type: struct {
        type: Type,
        inner_name: []const u8,
        is_defined: bool,
    },
    constant: struct {
        type: Type,
        value: Value,
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

    var lexer = Lexer.init(file, alloc, file_path) catch |err| return utils.printErr(
        error.FailedToTokenizeSource,
        "Failed to tokenize source code: {}\n",
        .{err},
        .red,
    );
    defer lexer.deinit(alloc);

    var parser = Parser.init(lexer, alloc) catch |err| return utils.printErr(
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

/// Switch to a different output section
pub fn switchSection(self: *Self, section: Section.Type) void {
    self.current_section = section;
}

/// Get current section
pub fn currentSection(self: *Self) *Section {
    return self.sections.getPtr(self.current_section);
}

/// Get writer for current section
pub fn currentWriter(self: *Self) *std.ArrayList(u8) {
    return &self.sections.getPtr(self.current_section).buffer;
}

/// Write to current section
pub fn write(self: *Self, bytes: []const u8) CompilerError!void {
    try self.currentWriter().insertSlice(self.alloc, self.currentSection().pos, bytes);
    self.currentSection().pos += bytes.len;
}

/// Print to current section
pub fn print(self: *Self, comptime fmt: []const u8, args: anytype) CompilerError!void {
    const bytes = try std.fmt.allocPrint(self.alloc, fmt, args);
    defer self.alloc.free(bytes);
    try self.write(bytes);
}

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
    _ = mode;
    const self = try alloc.create(Self);

    const visited = try alloc.create(std.ArrayList(Type.Context.Visited));
    visited.* = try .initCapacity(alloc, 128);

    self.* = .{
        .alloc = alloc,
        .input = input,
        .source_path = try alloc.dupe(u8, file_path),
        .module = try .init(alloc, b: {
            var module_name_it = std.mem.splitBackwardsAny(u8, self.source_path, "/.");
            _ = module_name_it.next().?;
            break :b module_name_it.next().?;
        }),
        .module_registry = registry,
        .exported_symbols = .init(alloc),
        .sections = .initFill(.{}),
        .current_section = .source_includes,
        .zag_header_contents = .initContext(alloc, .{ .visited = visited }),
        .pending_instantiations = .empty,
        .output_path = try std.fmt.allocPrint(alloc, "{s}.c", .{file_path}),
        .module_header_path = try std.fmt.allocPrint(alloc, "{s}.h", .{file_path}),
        .zag_header_path = try alloc.dupe(u8, "zag/zag.h"),
        .imported_modules = .init(alloc),
    };

    return self;
}

/// Frees resources
pub fn deinit(self: *Self) void {
    self.alloc.free(self.source_path);

    inline for (@typeInfo(Section.Type).@"enum".fields) |field| {
        const section = @field(Section.Type, field.name);
        self.sections.getPtr(section).buffer.deinit(self.alloc);
    }

    self.scopes.deinit(self.alloc);
    self.pending_instantiations.deinit(self.alloc);
    self.imported_modules.deinit();
}

/// Entry point for the compiler. Compiles AST into C code.
pub fn emit(self: *Self) CompilerError!void {
    try self.pushScope();
    defer self.popScope();

    // register constants
    try self.registerConstants();
    try self.scan();

    self.switchSection(.source_includes);
    try self.write("#include <zag.h>\n");
    try self.print("#include \"{s}\"\n", .{std.fs.path.basename(self.module_header_path)});

    // Pass 1: Imports
    for (self.input.items) |*statement| if (statement.* == .import)
        try statements.compile(self, statement);

    for (self.input.items) |*statement| switch (statement.*) {
        .struct_declaration,
        .union_declaration,
        .enum_declaration,
        .binding_type_declaration,
        .variable_definition,
        => try statements.compile(self, statement),
        else => {},
    };

    // Pass 3: Code (Functions, Variables, etc.)
    self.switchSection(.source_function_impls);
    for (self.input.items) |*statement| switch (statement.*) {
        .import,
        .struct_declaration,
        .union_declaration,
        .enum_declaration,
        .binding_type_declaration,
        .variable_definition,
        => {},
        else => try statements.compile(self, statement),
    };

    try self.solveGenerics();

    try self.writeOutputFiles();
}

fn writeOutputFiles(self: *Self) !void {
    // Create output directories
    var @".zag-out" = try std.fs.cwd().openDir(".zag-out", .{});
    defer @".zag-out".close();

    try @".zag-out".makePath(std.fs.path.dirname(self.output_path).?);
    try @".zag-out".makePath(std.fs.path.dirname(self.module_header_path).?);
    try @".zag-out".makePath(std.fs.path.dirname(self.zag_header_path).?);

    // Write main source file
    var output_file_buf: [1024]u8 = undefined;
    const output_file = try @".zag-out".createFile(self.output_path, .{});
    defer output_file.close();
    var output_file_writer = output_file.writer(&output_file_buf);

    try output_file_writer.interface.writeAll(self.sections.get(.source_includes).buffer.items);
    try output_file_writer.interface.writeAll(self.sections.get(.source_helper_functions).buffer.items);
    try output_file_writer.interface.writeAll(self.sections.get(.source_type_impls).buffer.items);
    try output_file_writer.interface.writeAll(self.sections.get(.source_function_impls).buffer.items);

    if (std.mem.eql(u8, self.source_path, "src/main.zag"))
        try output_file_writer.interface.writeAll("int main() { main_main(); return 0; }\n");

    try output_file_writer.interface.flush();

    // Write header file
    var header_file_buf: [1024]u8 = undefined;
    const header_file = try @".zag-out".createFile(self.module_header_path, .{});
    defer header_file.close();
    var header_file_writer = header_file.writer(&header_file_buf);

    const guard_name = try std.fmt.allocPrint(self.alloc, "__ZAG_MODULE_{}_H", .{std.hash.Wyhash.hash(0, self.source_path)});
    defer self.alloc.free(guard_name);

    try header_file_writer.interface.print("#ifndef {s}\n#define {s}\n\n", .{ guard_name, guard_name });
    try header_file_writer.interface.writeAll("#include <zag.h>\n\n");
    try header_file_writer.interface.writeAll(self.sections.get(.header_includes).buffer.items);
    try header_file_writer.interface.writeAll(self.sections.get(.header_type_defs).buffer.items);
    try header_file_writer.interface.writeAll(self.sections.get(.header_forward_decls).buffer.items);
    try header_file_writer.interface.writeAll(self.sections.get(.header_function_decls).buffer.items);
    try header_file_writer.interface.writeAll("\n#endif\n");
    try header_file_writer.interface.flush();
}

pub fn mangle(self: *const Self, name: []const u8) ![]const u8 {
    var module_name_it = std.mem.splitBackwardsAny(u8, self.source_path, "/.");
    _ = module_name_it.next().?;
    const module_name = module_name_it.next().?;
    return try std.fmt.allocPrint(self.alloc, "{s}_{s}", .{ module_name, name });
}

pub fn scan(self: *Self) CompilerError!void {
    for (self.input.items) |*statement| switch (statement.*) {
        .import => |*import_stmt| try self.registerSymbol(import_stmt.alias orelse import_stmt.module_name.getLast(), .{ .module = try self.processImport(import_stmt) }, .{}),
        .struct_declaration => |*struct_decl| try self.registerSymbol(
            struct_decl.name,
            .{ .type = .{ .@"struct" = try Type.Struct.init(self, struct_decl.name, try self.mangle(struct_decl.name), null) } },
            .{ .inner_name = try self.mangle(struct_decl.name), .is_defined = false },
        ),
        .union_declaration => |*union_decl| try self.registerSymbol(
            union_decl.name,
            .{ .type = .{ .@"union" = try Type.Union.init(self, union_decl.name, try self.mangle(union_decl.name), null) } },
            .{ .inner_name = try self.mangle(union_decl.name), .is_defined = false },
        ),
        .enum_declaration => |*enum_decl| try self.registerSymbol(
            enum_decl.name,
            .{ .type = .{ .@"enum" = try Type.Enum.init(self, enum_decl.name, try self.mangle(enum_decl.name), Type.getTagType(enum_decl.members.items.len)) } },
            .{ .inner_name = try self.mangle(enum_decl.name), .is_defined = false },
        ),
        .binding_type_declaration => |btd| try self.registerSymbol(btd.name, .{
            .type = switch (btd.type) {
                .@"struct" => .{ .@"struct" = try Type.Struct.init(self, btd.name, btd.name, null) },
                .@"union" => .{ .@"union" = try Type.Union.init(self, btd.name, btd.name, null) },
                .@"enum" => .{ .@"enum" = try Type.Enum.init(self, btd.name, btd.name, null) },
            },
        }, .{}),
        .variable_definition => |*var_def| {
            var final_type: Type = undefined;

            if (var_def.type != .inferred) {
                final_type = try .fromAst(self, var_def.type);
            } else {
                const received_type: Type = try .infer(self, var_def.assigned_value);
                if (received_type == .type) {
                    if (received_type.type) |inner_type| {
                        try self.registerSymbol(
                            var_def.variable_name,
                            .{ .type = inner_type.* },
                            .{
                                .is_defined = false,
                                .inner_name = switch (inner_type.*) {
                                    inline .@"struct", .@"union", .@"enum" => |ct| ct.inner_name,
                                    else => try self.mangle(var_def.variable_name),
                                },
                            },
                        );
                    }
                    continue;
                }

                final_type = received_type;
            }

            try self.registerSymbol(
                var_def.variable_name,
                .{ .symbol = .{ .is_mut = var_def.binding == .is_mut, .type = final_type } },
                .{ .is_defined = false, .inner_name = try self.mangle(var_def.variable_name) },
            );
        },
        else => {},
    };

    for (self.input.items) |*statement| switch (statement.*) {
        .struct_declaration => |*struct_decl| _ = try Type.fromCompoundTypeDeclaration(self, .@"struct", struct_decl),
        .union_declaration => |*union_decl| _ = try Type.fromCompoundTypeDeclaration(self, .@"union", union_decl),
        .enum_declaration => |*enum_decl| _ = try Type.fromCompoundTypeDeclaration(self, .@"enum", enum_decl),
        .function_definition => |*func| {
            var t = try Type.fromAst(self, func.getType());
            t.function.definition = func;
            try self.registerSymbol(
                func.name,
                .{ .symbol = .{ .type = t } },
                .{
                    .inner_name = try self.mangle(func.name),
                    .is_defined = false,
                },
            );
        },
        .binding_function_declaration => |*func| {
            var t: Type = try .fromAst(self, func.getType());
            t.function.is_bind = true;
            try self.registerSymbol(func.name, .{
                .symbol = .{ .type = t },
            }, .{ .is_defined = false });
        },
        else => {},
    };
}

pub fn analyze(self: *Self) CompilerError!void {
    try self.pushScope();
    defer self.popScope();

    try self.registerConstants();

    for (self.input.items) |*statement| {
        switch (statement.*) {
            .import => |*import_stmt| {
                const mod = try self.processImport(import_stmt);
                const name = import_stmt.alias orelse import_stmt.module_name.getLast();
                try self.registerSymbol(name, .{ .module = mod }, .{});
                try self.imported_modules.put(name, mod);
            },
            .function_definition => |*func| {
                var t = try Type.fromAst(self, func.getType());
                t.function.definition = func;
                try self.registerSymbol(
                    func.name,
                    .{ .symbol = .{ .type = t } },
                    .{ .is_defined = false, .inner_name = try self.mangle(func.name) },
                );

                if (func.is_pub) try self.exported_symbols.put(func.name, .{
                    .name = func.name,
                    .inner_name = try self.mangle(func.name),
                    .is_pub = func.is_pub,
                    .type = t,
                });
            },
            .binding_function_declaration => |*func| {
                var t = try Type.fromAst(self, func.getType());
                t.function.is_bind = true;
                try self.registerSymbol(func.name, .{ .symbol = .{ .type = t } }, .{ .is_defined = false });
                if (func.is_pub) try self.exported_symbols.put(func.name, .{
                    .name = func.name,
                    .inner_name = func.name,
                    .is_pub = func.is_pub,
                    .type = t,
                });
            },
            .variable_definition => |*var_def| {
                // for variables, we need to infer type if possible or take declared type
                // logic similar to variableDefinition in statements.zig
                const received_type: Type = try .infer(self, var_def.assigned_value);
                const expected_type: ?Type = if (var_def.type == .inferred) null else try .fromAst(self, var_def.type);

                const final_type = expected_type orelse received_type;

                try self.registerSymbol(
                    var_def.variable_name,
                    .{ .symbol = .{ .is_mut = var_def.binding == .is_mut, .type = final_type } },
                    .{
                        .is_defined = false,
                        .inner_name = try self.mangle(var_def.variable_name),
                    },
                );

                if (var_def.is_pub) try self.exported_symbols.put(var_def.variable_name, .{
                    .name = var_def.variable_name,
                    .inner_name = try self.mangle(var_def.variable_name),
                    .is_pub = var_def.is_pub,
                    .type = final_type,
                    .is_mut = var_def.binding == .is_mut,
                });
            },
            .binding_type_declaration => |btd| {
                const t: Type = switch (btd.type) {
                    .@"struct" => .{ .@"struct" = try Type.Struct.init(self, btd.name, btd.name, null) },
                    .@"union" => .{ .@"union" = try Type.Union.init(self, btd.name, btd.name, null) },
                    .@"enum" => .{ .@"enum" = try Type.Enum.init(self, btd.name, btd.name, null) },
                };

                try self.registerSymbol(btd.name, .{ .type = t }, .{});

                if (btd.is_pub) try self.exported_symbols.put(btd.name, .{
                    .name = btd.name,
                    .inner_name = btd.name,
                    .is_pub = btd.is_pub,
                    .type = t,
                });
            },
            inline .struct_declaration, .union_declaration, .enum_declaration => |*struct_decl| {
                const compound_type = try Type.fromCompoundTypeDeclaration(self, switch (@TypeOf(struct_decl.*)) {
                    ast.Statement.StructDeclaration => .@"struct",
                    ast.Statement.UnionDeclaration => .@"union",
                    ast.Statement.EnumDeclaration => .@"enum",
                    else => unreachable,
                }, struct_decl);

                const symbol_type = @unionInit(Type, switch (@TypeOf(struct_decl.*)) {
                    ast.Statement.StructDeclaration => "struct",
                    ast.Statement.UnionDeclaration => "union",
                    ast.Statement.EnumDeclaration => "enum",
                    else => unreachable,
                }, compound_type);

                // Exported symbols logic
                if (struct_decl.is_pub) try self.exported_symbols.put(struct_decl.name, .{
                    .name = struct_decl.name,
                    .inner_name = try self.mangle(struct_decl.name),
                    .is_pub = struct_decl.is_pub,
                    .type = symbol_type,
                });
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

    // Check registry
    if (self.module_registry.get(full_path)) |mod| return mod;

    // Analyze new module
    const ast_res = try getAST(self.alloc, full_path); // recursive AST

    // We pass the SAME registry
    var child_compiler = try init(self.alloc, ast_res.root, full_path, self.module_registry, .analysis);
    defer child_compiler.deinit();

    try child_compiler.analyze();

    // Create module from exports
    // Allocate on heap to ensure pointer stability
    var mod: Module = try .init(self.alloc, import_stmt.alias orelse import_stmt.module_name.getLast());

    var it = child_compiler.exported_symbols.iterator();
    while (it.next()) |entry| {
        var sym = entry.value_ptr.*;
        switch (sym.type) {
            .@"struct" => |*s| s.module = mod,
            .@"union" => |*u| u.module = mod,
            .function => |*f| f.module = mod,
            else => {},
        }
        try mod.symbols.put(entry.key_ptr.*, sym);
    }

    var imp_it = child_compiler.imported_modules.iterator();
    while (imp_it.next()) |entry|
        try mod.imports.put(entry.key_ptr.*, entry.value_ptr.*);

    // Register
    try self.module_registry.put(full_path, mod);

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
        return_implicit_success: ?Type = null,
    },
) CompilerError!void {
    try self.pushScope();
    defer self.popScope();

    try self.write("{\n");

    if (opts.capture) |capture| {
        const inner_type = (try Type.infer(self, capture.condition.*)).optional.*;

        try self.compileVariableSignature(capture.name, inner_type, .{});
        try self.write(" = (");
        try expressions.compile(self, capture.condition, .{});
        try self.write(").payload;\n");

        try self.registerSymbol(capture.name, .{ .symbol = .{ .type = inner_type } }, .{});
    }

    if (opts.iterator) |iterator| {
        const t: Type = try .infer(self, iterator.iter_expr.*);
        const inner_type = switch (t) {
            inline .array, .slice => |ti| ti.inner.*,
            else => unreachable,
        };

        try self.compileVariableSignature(iterator.capture_name, inner_type, .{});
        try self.write(" = (");
        try expressions.compile(self, iterator.iter_expr, .{});
        try self.write(")");
        if (t == .slice) try self.write(".ptr");
        try self.print("[{s}];\n", .{iterator.index});

        try self.registerSymbol(iterator.capture_name, .{ .symbol = .{ .type = inner_type } }, .{});
    }

    var return_expr: ?ast.Expression = null;
    for (block.items) |*statement| switch (statement.*) {
        .@"return" => |r| {
            return_expr = r.@"return";
            break;
        },
        else => try statements.compile(self, statement),
    };

    if (return_expr) |e| {
        try compileType(self, self.current_return_type.?, .{});
        try self.write(" __zag_ret_val = ");
        try expressions.compile(self, &e, .{
            .is_variable_declaration = true,
            .expected_type = self.current_return_type.?,
        });
        try self.write(";\n");
    }

    for (self.scopes.getLast().pending_defers.items) |pd| try statements.compile(self, pd);

    if (return_expr) |_| {
        try self.write("return __zag_ret_val;");
    } else if (opts.return_implicit_success) |ris| {
        try self.write("return (");
        try self.compileType(ris, .{});
        try self.write("){ .is_success = true, .payload.success = 0 };\n");
    }

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
    if (opts.is_top_level and !opts.binding_mut and t != .reference and t != .variadic) try self.write("const ");
    const new_opts: @TypeOf(opts) = .{ .binding_mut = opts.binding_mut, .is_top_level = false };

    return switch (t) {
        .reference => |reference| {
            if (!reference.is_mut) try self.write("const ");

            try self.compileType(reference.inner.*, new_opts);
            try self.write(" *");

            if (!opts.binding_mut) try self.write(" const");
        },
        .@"struct" => |s| try self.write(s.inner_name),
        .@"union" => |u| try self.write(u.inner_name),
        .@"enum" => |e| try self.write(e.inner_name),
        .optional => |optional| {
            const type_name = try std.fmt.allocPrint(self.alloc, "__zag_Optional_{}", .{t.hash()});
            if (self.zag_header_contents.get(t) == null) {
                const saved_section = self.current_section;
                self.switchSection(.zag_header_types);
                defer self.switchSection(saved_section);

                try self.write("typedef struct {\n");
                try self.write("bool is_some;\n");
                try self.compileType(optional.*, new_opts);
                try self.write(" payload;\n");
                try self.print("}} {s};\n", .{type_name});

                try self.zag_header_contents.put(t, type_name);
            }

            try self.write(type_name);
        },
        .error_union => |error_union| {
            const type_name = try std.fmt.allocPrint(self.alloc, "__zag_ErrorUnion_{}", .{t.hash()});
            if (self.zag_header_contents.get(t) == null) {
                const saved_section = self.current_section;
                self.switchSection(.zag_header_types);
                defer self.switchSection(saved_section);

                try self.write("typedef struct {\n");
                try self.write("bool is_success;\n");
                try self.write("union {\n");
                // if the success type is void, use u8 as a small dummy type since C union members can't be void
                try self.compileType(switch (error_union.success.*) {
                    .void => .u8,
                    else => |other| other,
                }, new_opts);
                try self.write(" success;\n");
                try self.compileType(error_union.failure.*, new_opts);
                try self.write(" failure;\n");
                try self.write("} payload;\n");
                try self.print("}} {s};\n", .{type_name});

                try self.zag_header_contents.put(t, type_name);
            }

            try self.write(type_name);
        },
        .slice => |slice| {
            const type_name = try std.fmt.allocPrint(self.alloc, "__zag_Slice_{}", .{t.hash()});
            if (self.zag_header_contents.get(t) == null) {
                const saved_section = self.current_section;
                self.switchSection(.zag_header_types);
                defer self.switchSection(saved_section);

                try self.write("typedef struct {\n");
                try self.compileType(slice.inner.*, .{
                    .binding_mut = slice.is_mut,
                    .is_top_level = true,
                });
                try self.write(" *ptr;\n");
                try self.write("size_t len;\n");
                try self.print("}} {s};\n", .{type_name});

                try self.zag_header_contents.put(t, type_name);
            }

            try self.write(type_name);
        },
        .array => |array| {
            try self.compileType(array.inner.*, new_opts);
            try self.print("[{}]", .{array.size});
        },

        .function => |function| {
            const type_name = try std.fmt.allocPrint(self.alloc, "__zag_FuncPtr_{}", .{t.hash()});

            if (self.zag_header_contents.get(t) == null) {
                const saved_section = self.current_section;
                self.switchSection(.zag_header_types);
                defer self.switchSection(saved_section);

                try self.write("typedef ");
                try self.compileType(function.return_type.*, .{ .binding_mut = true, .is_top_level = new_opts.is_top_level });
                try self.print(" (*{s})(", .{type_name});
                for (function.params.items, 0..) |param, i| {
                    try self.compileType(param.type, new_opts);
                    if (i < function.params.items.len - 1) try self.write(", ");
                }
                try self.write(");\n");

                try self.zag_header_contents.put(t, type_name);
            }

            try self.write(type_name);
        },
        .variadic => try self.write("..."),
        .any => try self.write("void*"),
        else => |primitive| try self.write(try self.getInnerName(@tagName(primitive))),
    };
}

pub fn compileVariableSignature(
    self: *Self,
    name: []const u8,
    t: Type,
    opts: struct {
        binding_mut: bool = false,
        is_const: bool = false,
    },
) CompilerError!void {
    if (t == .type) unreachable;

    if (opts.is_const) try self.write("static ");

    switch (t) {
        .array => |array| {
            try self.compileType(array.inner.*, .{ .binding_mut = opts.binding_mut });
            try self.print(" {s}[{}]", .{ name, array.size });
        },
        .function => |function| {
            if (self.zag_header_contents.get(t)) |type_name| {
                try self.print("{s} {s}", .{ type_name, name });
                return;
            }

            try self.compileType(function.return_type.*, .{ .binding_mut = true });
            try self.print(" (*{s})(", .{name});
            for (function.params.items, 0..) |param, i| {
                try self.compileType(param.type, .{ .binding_mut = opts.binding_mut });
                if (i < function.params.items.len - 1) try self.write(", ");
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
    errdefer std.debug.dumpCurrentStackTrace(null);
    return switch (expression) {
        .int => |int| .{ .i64 = int.int },
        .uint => |uint| .{ .u64 = uint.uint },
        .float => |float| .{ .f64 = float.float },
        .char => |char| .{ .u8 = char.char },
        .binary => |binary| try (try self.solveComptimeExpression(binary.lhs.*))
            .binaryOperation(binary.op, try self.solveComptimeExpression(binary.rhs.*)),
        .ident => |ident| b: {
            const item = self.getScopeItem(ident.ident) catch |err| switch (err) {
                error.UnknownSymbol => return err, // Or handle primitive types if not in scope?
                else => return err,
            };
            break :b switch (item) {
                .constant => |c| c.value,
                .type => |t| if (t.type == .generic_param)
                    return error.ExpressionCannotBeEvaluatedAtCompileTime
                else
                    .{ .type = t.type },
                .module => |m| .{ .type = .{ .module = m } }, // Module as value? Type?
                .symbol => return error.ExpressionCannotBeEvaluatedAtCompileTime, // Variable cannot be evaluated at comptime (unless const?)
            };
        },
        .generic => .{ .type = try .infer(self, expression) },
        .type => |t| .{ .type = try .fromAst(self, t) },
        .prefix => |prefix| switch (prefix.op) {
            .@"-" => switch (try solveComptimeExpression(self, prefix.rhs.*)) {
                inline .i64, .u64, .f64 => |number, tag| @unionInit(
                    Value,
                    if (tag == .u64) "i64" else @tagName(tag),
                    if (tag == .u64) -@as(i64, @intCast(number)) else -number,
                ),
                else => |other| errors.illegalPrefixExpression(prefix.op, other.getType(), expression.getPosition()),
            },
            .@"!" => switch (try solveComptimeExpression(self, prefix.rhs.*)) {
                .bool => |b| .{ .bool = !b },
                else => |other| errors.illegalPrefixExpression(prefix.op, other.getType(), expression.getPosition()),
            },
        },
        .member => .{ .type = try .infer(self, expression) },
        else => return error.ExpressionCannotBeEvaluatedAtCompileTime,
    };
}

fn solveGenerics(self: *Self) !void {
    // this is a while loop because the iterator grows, so a for loop won't work
    var processed: usize = 0;
    while (processed < self.pending_instantiations.items.len) {
        const instantiation = self.pending_instantiations.items[processed];
        processed += 1;

        // Skip template emission
        var is_template = false;
        for (instantiation.args) |arg| {
            if (arg == .type and arg.type == .generic_param) {
                is_template = true;
                break;
            }
        }
        if (is_template) continue;

        try self.pushScope();
        defer self.popScope();

        var it = instantiation.module.symbols.iterator();
        while (it.next()) |entry|
            try self.scopes.getLast().items.put(entry.key_ptr.*, .{ .symbol = .{
                .type = entry.value_ptr.type,
                .inner_name = entry.value_ptr.name,
                .is_mut = false,
                .is_defined = true,
            } });

        var imp_it = instantiation.module.imports.iterator();
        while (imp_it.next()) |entry|
            try self.registerSymbol(entry.key_ptr.*, .{ .module = entry.value_ptr.* }, .{});

        switch (instantiation.t) {
            inline .@"struct", .@"union" => |s, tag| {
                for (s.generic_types.items, 0..) |param, j| {
                    const val = instantiation.args[j];
                    switch (val) {
                        .type => |t| try self.registerSymbol(param.name, .{ .type = t }, .{}),
                        else => try self.registerSymbol(param.name, .{
                            .constant = .{
                                .type = val.getType(),
                                .value = val,
                            },
                        }, .{}),
                    }
                }

                try statements.compile(self, &@unionInit(
                    ast.Statement,
                    @tagName(tag) ++ "_declaration",
                    .{
                        .name = instantiation.inner_name,
                        .generic_types = .empty,
                        .is_pub = true,
                        .pos = s.pos,
                        .variables = s.variables,
                        .subtypes = s.subtypes,
                        .members = s.members,
                        .methods = s.methods,
                    },
                ));
            },
            .function => |f| {
                for (f.generic_parameters.items, 0..) |param, j| {
                    const val = instantiation.args[j];
                    switch (val) {
                        .type => |t| try self.registerSymbol(param.name, .{ .type = t }, .{}),
                        else => try self.registerSymbol(param.name, .{
                            .constant = .{
                                .type = val.getType(),
                                .value = val,
                            },
                        }, .{}),
                    }
                }

                try statements.compile(self, &.{
                    .function_definition = .{
                        .name = instantiation.inner_name,
                        .generic_parameters = .empty,
                        .is_pub = true,
                        .pos = f.pos,
                        .parameters = f.parameters,
                        .return_type = f.return_type,
                        .body = f.body,
                    },
                });
            },
        }
    }
}

/// appends a new empty scope to the scope stack.
pub fn pushScope(self: *Self) !void {
    const items = try self.alloc.create(std.StringHashMap(ScopeItem));
    items.* = .init(self.alloc);

    const pending_defers = try self.alloc.create(std.ArrayList(*const ast.Statement));
    pending_defers.* = .empty;

    try self.scopes.append(self.alloc, .{
        .pending_defers = pending_defers,
        .items = items,
    });
}

/// pops the scope of the scope stack.
pub fn popScope(self: *Self) void {
    var last = self.scopes.pop().?;
    last.pending_defers.deinit(self.alloc);
    last.items.deinit();
}

/// registers a new entry in the top scope of the scope stack.
pub fn registerSymbol(
    self: *Self,
    name: []const u8,
    symbol_or_type: union(enum) {
        symbol: struct {
            is_mut: bool = false,
            type: Type,
        },
        type: Type,
        module: Module,
        constant: struct { type: Type, value: Value },
    },
    opts: struct {
        inner_name: ?[]const u8 = null,
        is_defined: bool = true,
    },
) !void {
    try self.scopes.getLast().items.put(name, switch (symbol_or_type) {
        .symbol => |symbol| .{
            .symbol = .{
                .is_mut = symbol.is_mut,
                .is_defined = opts.is_defined,
                .type = symbol.type,
                .inner_name = opts.inner_name orelse name,
            },
        },
        .type => |@"type"| .{
            .type = .{
                .type = @"type",
                .is_defined = opts.is_defined,
                .inner_name = opts.inner_name orelse name,
            },
        },
        .module => |module| .{ .module = module },
        .constant => |constant| .{
            .constant = .{
                .type = constant.type,
                .value = constant.value,
                .inner_name = opts.inner_name orelse name,
            },
        },
    });
}

/// Registers boolean constants, null, undefined, and primitive types
fn registerConstants(self: *Self) !void {
    try self.registerSymbol("true", .{ .symbol = .{ .type = .bool } }, .{});
    try self.registerSymbol("false", .{ .symbol = .{ .type = .bool } }, .{});
    try self.registerSymbol("null", .{ .symbol = .{ .type = .@"typeof(null)" } }, .{});
    try self.registerSymbol("undefined", .{ .symbol = .{ .type = .@"typeof(undefined)" } }, .{});

    try self.registerSymbol("i8", .{ .type = .i8 }, .{ .inner_name = "int8_t" });
    try self.registerSymbol("i16", .{ .type = .i16 }, .{ .inner_name = "int16_t" });
    try self.registerSymbol("i32", .{ .type = .i32 }, .{ .inner_name = "int32_t" });
    try self.registerSymbol("i64", .{ .type = .i64 }, .{ .inner_name = "int64_t" });

    try self.registerSymbol("u8", .{ .type = .u8 }, .{ .inner_name = "uint8_t" });
    try self.registerSymbol("u16", .{ .type = .u16 }, .{ .inner_name = "uint16_t" });
    try self.registerSymbol("u32", .{ .type = .u32 }, .{ .inner_name = "uint32_t" });
    try self.registerSymbol("u64", .{ .type = .u64 }, .{ .inner_name = "uint64_t" });

    try self.registerSymbol("usize", .{ .type = .usize }, .{ .inner_name = "size_t" });

    try self.registerSymbol("f32", .{ .type = .f32 }, .{ .inner_name = "float" });
    try self.registerSymbol("f64", .{ .type = .f64 }, .{ .inner_name = "double" });

    try self.registerSymbol("void", .{ .type = .void }, .{});
    try self.registerSymbol("bool", .{ .type = .bool }, .{});
    try self.registerSymbol("any", .{ .type = .any }, .{});

    try self.registerSymbol("type", .{ .type = .{ .type = null } }, .{});

    try self.registerSymbol("c_int", .{ .type = .c_int }, .{ .inner_name = "int" });
    try self.registerSymbol("c_char", .{ .type = .c_char }, .{ .inner_name = "char" });
    try self.registerSymbol("c_long", .{ .type = .c_long }, .{ .inner_name = "long" });
    try self.registerSymbol("c_short", .{ .type = .c_short }, .{ .inner_name = "short" });

    try self.registerSymbol("c_uint", .{ .type = .c_uint }, .{ .inner_name = "unsigned int" });
    try self.registerSymbol("c_uchar", .{ .type = .c_uchar }, .{ .inner_name = "unsigned char" });
    try self.registerSymbol("c_ulong", .{ .type = .c_ulong }, .{ .inner_name = "unsigned long" });
    try self.registerSymbol("c_ushort", .{ .type = .c_ushort }, .{ .inner_name = "unsigned short" });

    try self.registerSymbol("c_null", .{ .symbol = .{ .type = .{ .reference = .{ .inner = &.void, .is_mut = false } } } }, .{ .inner_name = "NULL" });

    const builtins: Module = try .init(self.alloc, "builtins");

    try self.registerSymbol("sizeof", .{
        .symbol = .{
            .is_mut = false,
            .type = .{
                .function = .{
                    .name = "sizeof",
                    .inner_name = "sizeof",
                    .generic_params = b: {
                        var params: std.ArrayList(Type.Function.Param) = .empty;
                        try params.append(self.alloc, .{ .name = "T", .type = .{ .type = null } });
                        break :b params;
                    },
                    .return_type = b: {
                        const t = try self.alloc.create(Type);
                        t.* = .usize;
                        break :b t;
                    },
                    .params = .empty,
                    .module = builtins,
                },
            },
        },
    }, .{});

    try self.registerSymbol("cast", .{
        .symbol = .{
            .is_mut = false,
            .type = .{
                .function = .{
                    .name = "cast",
                    .inner_name = "cast",
                    .generic_params = b: {
                        var params: std.ArrayList(Type.Function.Param) = .empty;
                        try params.append(self.alloc, .{ .name = "T", .type = .{ .type = null } });
                        break :b params;
                    },
                    .return_type = b: {
                        const t = try self.alloc.create(Type);
                        t.* = .usize; // Default, will be overridden
                        break :b t;
                    },
                    .params = b: {
                        var params: std.ArrayList(Type.Function.Param) = .empty;
                        try params.append(self.alloc, .{ .name = "val", .type = .any });
                        break :b params;
                    },
                    .module = builtins,
                },
            },
        },
    }, .{});

    try self.registerSymbol("xor", .{
        .symbol = .{
            .is_mut = false,
            .type = .{
                .function = .{
                    .name = "xor",
                    .inner_name = "xor",
                    .generic_params = b: {
                        var params: std.ArrayList(Type.Function.Param) = .empty;
                        try params.append(self.alloc, .{ .name = "T", .type = .{ .type = null } });
                        break :b params;
                    },
                    .return_type = b: {
                        const t = try self.alloc.create(Type);
                        t.* = .usize; // Default, will be overridden
                        break :b t;
                    },
                    .params = b: {
                        var params: std.ArrayList(Type.Function.Param) = .empty;
                        try params.append(self.alloc, .{ .name = "a", .type = .any });
                        try params.append(self.alloc, .{ .name = "b", .type = .any });
                        break :b params;
                    },
                    .module = builtins,
                },
            },
        },
    }, .{});
}

pub fn getSymbolType(self: *const Self, symbol: []const u8) !Type {
    return switch (try self.getScopeItem(symbol)) {
        .module => |module| .{ .module = module },
        .constant => |c| c.type,
        inline .symbol, .type => |s| s.type,
    };
}

pub fn getScopeItem(self: *const Self, symbol: []const u8) !ScopeItem {
    var it = std.mem.reverseIterator(self.scopes.items);
    while (it.next()) |scope| return scope.items.get(symbol) orelse continue;

    return error.UnknownSymbol;
}

pub fn getSymbolMutability(self: *const Self, symbol: []const u8) !bool {
    return switch (try self.getScopeItem(symbol)) {
        .symbol => |s| s.is_mut,
        else => {
            std.debug.dumpCurrentStackTrace(null);
            return utils.printErr(
                error.SymbolNotVariable,
                "Compiler error: Symbol '{s}' is not a variable.\n",
                .{symbol},
                .red,
            );
        },
    };
}

pub fn getSymbolDefined(self: *const Self, symbol: []const u8) !bool {
    return switch (try self.getScopeItem(symbol)) {
        .symbol => |s| s.is_defined,
        .type => |t| t.is_defined,
        else => return utils.printErr(
            error.SymbolNotVariable,
            "Compiler error: Symbol {s} is not a variable or function.\n",
            .{symbol},
            .red,
        ),
    };
}

pub fn getInnerName(self: *const Self, symbol: []const u8) ![]const u8 {
    var it = std.mem.reverseIterator(self.scopes.items);
    while (it.next()) |scope| {
        if (scope.items.get(symbol)) |item| {
            return switch (item) {
                .module => |module| module.name,
                .constant => utils.printErr(
                    error.SymbolNotVariable,
                    "comperr: Symbol '{s}' is a constant, not a variable\n",
                    .{symbol},
                    .red,
                ),
                inline else => |s| s.inner_name,
            };
        }
    }

    // Search in modules in scope
    it = std.mem.reverseIterator(self.scopes.items);
    while (it.next()) |scope| {
        var scope_it = scope.items.iterator();
        while (scope_it.next()) |entry| {
            switch (entry.value_ptr.*) {
                .module => |module| if (module.symbols.get(symbol)) |s| return s.inner_name,
                else => {},
            }
        }
    }

    return utils.printErr(
        error.UnknownSymbol,
        "comperr: Unknown symbol '{s}'.\n",
        .{symbol},
        .red,
    );
}
