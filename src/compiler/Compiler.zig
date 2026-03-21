const std = @import("std");
const build_options = @import("build_options");

const utils = @import("utils");
const ast = @import("ast");

const analysis = @import("analysis.zig");
const expressions = @import("expressions.zig");
const statements = @import("statements.zig");
const errors = @import("errors.zig");

const Lexer = @import("Lexer");
const Parser = @import("Parser");

const Type = @import("types/Type.zig").Type;
const Value = @import("Value.zig").Value;

const Self = @This();

pub const Module = @import("Module.zig");

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
        header_primitives,

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

type_def_blocks: std.ArrayList(TypeDefBlock) = .empty,
type_def_stack: std.ArrayList(usize) = .empty,

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
    is_function_boundary: bool = false,
    is_loop_body: bool = false,
};
pub const ScopeItem = union(enum) {
    symbol: struct {
        type: Type,
        is_mut: bool,
        inner_name: []const u8,
        is_defined: bool,
        should_free: bool,
    },
    type: struct {
        type: Type,
        inner_name: []const u8,
        is_defined: bool,
        should_free: bool,
    },
    constant: struct {
        type: Type,
        value: Value,
        inner_name: []const u8,
        should_free: bool,
    },
    module: Module,
};

pub const TypeDefBlock = struct {
    type: Type,
    code: std.ArrayList(u8),
    saved_section: Section.Type,
};

pub fn getAST(
    alloc: std.mem.Allocator,
    file_path: []const u8,
) CompilerError!struct {
    root: ast.RootNode,
    source: []u8,
} {
    const file = std.fs.cwd().readFileAlloc(alloc, file_path, 1 << 20) catch |err| return utils.printErr(
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
        .root = try utils.cloneSlice(ast.Statement, parser.output, alloc),
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
    if (self.type_def_stack.items.len > 0 and self.current_section == .header_type_defs) {
        const idx = self.type_def_stack.getLast();
        try self.type_def_blocks.items[idx].code.appendSlice(self.alloc, bytes);
        return;
    }
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
) CompilerError!*Self {
    const self = try alloc.create(Self);

    const visited = try alloc.create(std.ArrayList(Type.Context.Visited));
    visited.* = try .initCapacity(alloc, 128);

    const relative_path = if (std.mem.startsWith(u8, file_path, build_options.stdlib_path))
        try std.fmt.allocPrint(alloc, "lib/{s}", .{file_path[build_options.stdlib_path.len + 1 ..]})
    else
        file_path;

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
        .output_path = try std.fmt.allocPrint(alloc, "{s}.c", .{relative_path}),
        .module_header_path = try std.fmt.allocPrint(alloc, "{s}.h", .{relative_path}),
        .zag_header_path = "zag/zag.h",
        .imported_modules = .init(alloc),
    };

    return self;
}

/// Frees resources
pub fn deinit(self: *Self) void {
    utils.deinitSlice(ast.Statement, self.input, self.alloc);

    self.alloc.free(self.source_path);

    inline for (@typeInfo(Section.Type).@"enum".fields) |field| {
        const section = @field(Section.Type, field.name);
        self.sections.getPtr(section).buffer.deinit(self.alloc);
    }

    for (self.type_def_blocks.items) |*block| block.code.deinit(self.alloc);
    self.type_def_blocks.deinit(self.alloc);
    self.type_def_stack.deinit(self.alloc);

    for (self.scopes.items) |i| {
        var it = i.items.iterator();
        while (it.next()) |scope| {
            switch (scope.value_ptr.*) {
                .module => {},
                .symbol => |s| {
                    if (s.should_free) self.alloc.free(s.inner_name);
                    s.type.deinit(self.alloc);
                },
                .type => |t| {
                    if (t.should_free) self.alloc.free(t.inner_name);
                    t.type.deinit(self.alloc);
                },
                .constant => |c| {
                    if (c.should_free) self.alloc.free(c.inner_name);
                    c.type.deinit(self.alloc);
                    c.value.deinit(self.alloc);
                },
            }
        }
        i.items.deinit();
        self.alloc.destroy(i.items);
    }
    self.scopes.deinit(self.alloc);

    for (self.pending_instantiations.items) |inst| {
        self.alloc.free(inst.inner_name);
        utils.deinitSlice(Value, inst.args, self.alloc);
    }
    self.pending_instantiations.deinit(self.alloc);

    var import_mods_it = self.imported_modules.valueIterator();
    while (import_mods_it.next()) |mod| mod.deinit(self.alloc);
    self.imported_modules.deinit();
    self.module.deinit(self.alloc);
}

/// Entry point for the compiler. Compiles AST into C code.
pub fn emit(self: *Self) CompilerError!void {
    try self.pushScope(false);
    defer self.popScope();

    // register constants
    try self.registerConstants();
    try self.scan();

    self.switchSection(.source_includes);
    try self.write("#include <zag.h>\n");
    try self.print("#include \"{s}\"\n", .{std.fs.path.basename(self.module_header_path)});

    // Pass 1: Imports
    for (self.input) |*statement| if (statement.* == .import)
        try statements.compile(self, statement);

    for (self.input) |*statement| switch (statement.*) {
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
    for (self.input) |*statement| switch (statement.*) {
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
    var @".zag-out" = try std.fs.cwd().makeOpenPath(".zag-out", .{});
    defer @".zag-out".close();

    if (std.fs.path.dirname(self.output_path)) |dir| try @".zag-out".makePath(dir);
    if (std.fs.path.dirname(self.module_header_path)) |dir| try @".zag-out".makePath(dir);
    if (std.fs.path.dirname(self.zag_header_path)) |dir| try @".zag-out".makePath(dir);

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

    const guard_name = try std.fmt.allocPrint(self.alloc, "__ZAG_MODULE_{x}_H", .{std.hash.Wyhash.hash(0, self.source_path)});
    defer self.alloc.free(guard_name);

    try header_file_writer.interface.print("#ifndef {s}\n#define {s}\n\n", .{ guard_name, guard_name });
    try header_file_writer.interface.writeAll("#include <zag.h>\n\n");
    try header_file_writer.interface.writeAll(self.sections.get(.header_includes).buffer.items);
    try header_file_writer.interface.writeAll(self.sections.get(.header_forward_decls).buffer.items);
    try header_file_writer.interface.writeAll(self.sections.get(.header_primitives).buffer.items);
    try self.emitTypeDefsInOrder(&header_file_writer.interface);
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

fn processFunctionDefinitionSymbol(self: *Self, func: *const ast.Statement.FunctionDefinition) !void {
    var t = try Type.fromAst(self, func.getType());
    t.function.definition = func;
    try self.registerSymbol(
        func.name,
        .{ .symbol = .{ .type = t } },
        .{
            .inner_name = try self.mangle(func.name),
            .should_free = true,
            .is_defined = false,
        },
    );
}

pub fn scan(self: *Self) CompilerError!void {
    for (self.input) |*statement| switch (statement.*) {
        .import => |*import_stmt| try self.registerSymbol(
            import_stmt.alias orelse import_stmt.module_name[import_stmt.module_name.len - 1],
            .{ .module = try self.processImport(import_stmt) },
            .{},
        ),
        .struct_declaration => |*struct_decl| {
            try self.registerSymbol(
                struct_decl.name,
                .{ .type = .{ .@"struct" = try Type.Struct.init(self, struct_decl.name, try self.mangle(struct_decl.name), null) } },
                .{ .inner_name = try self.mangle(struct_decl.name), .is_defined = false },
            );
        },
        .union_declaration => |*union_decl| try self.registerSymbol(
            union_decl.name,
            .{ .type = .{ .@"union" = try Type.Union.init(self, union_decl.name, try self.mangle(union_decl.name), null) } },
            .{ .inner_name = try self.mangle(union_decl.name), .is_defined = false },
        ),
        .enum_declaration => |*enum_decl| try self.registerSymbol(
            enum_decl.name,
            .{ .type = .{ .@"enum" = try Type.Enum.init(self, enum_decl.name, try self.mangle(enum_decl.name), Type.getTagType(enum_decl.members.len)) } },
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
                    try self.registerSymbol(
                        var_def.variable_name,
                        .{ .type = received_type.type.* },
                        .{
                            .is_defined = false,
                            .inner_name = switch (received_type.type.*) {
                                inline .@"struct", .@"union", .@"enum" => |ct| ct.inner_name,
                                else => try self.mangle(var_def.variable_name),
                            },
                        },
                    );
                    continue;
                }

                final_type = received_type;
            }

            try self.registerSymbol(
                var_def.variable_name,
                .{ .symbol = .{ .is_mut = var_def.binding == .let_mut, .type = final_type } },
                .{ .is_defined = false, .inner_name = try self.mangle(var_def.variable_name) },
            );
        },
        else => {},
    };

    for (self.input) |*statement| switch (statement.*) {
        .struct_declaration => |*struct_decl| _ = try Type.fromCompoundTypeDeclaration(self, .@"struct", struct_decl, .{}),
        .union_declaration => |*union_decl| _ = try Type.fromCompoundTypeDeclaration(self, .@"union", union_decl, .{}),
        .enum_declaration => |*enum_decl| _ = try Type.fromCompoundTypeDeclaration(self, .@"enum", enum_decl, .{}),
        .function_definition => |*func| try processFunctionDefinitionSymbol(self, func),
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
    try self.pushScope(false);
    defer self.popScope();

    try self.registerConstants();
    try self.scan();

    for (self.input) |*statement| {
        switch (statement.*) {
            .import => |*import_stmt| {
                const mod = try self.processImport(import_stmt);
                const name = import_stmt.alias orelse import_stmt.module_name[import_stmt.module_name.len - 1];
                try self.registerSymbol(name, .{ .module = mod }, .{});
                try self.imported_modules.put(name, mod);
            },
            .function_definition => |*func| {
                try processFunctionDefinitionSymbol(self, func);

                try self.exported_symbols.put(func.name, .{
                    .name = func.name,
                    .inner_name = try self.mangle(func.name),
                    .is_pub = func.is_pub,
                    .type = try self.getSymbolType(func.name),
                });
            },
            .binding_function_declaration => |*func| {
                var t = try Type.fromAst(self, func.getType());
                t.function.is_bind = true;
                try self.registerSymbol(func.name, .{ .symbol = .{ .type = t } }, .{ .is_defined = false });
                try self.exported_symbols.put(func.name, .{
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
                defer if (expected_type) |et| et.deinit(self.alloc);

                const final_type = expected_type orelse received_type;

                try self.registerSymbol(
                    var_def.variable_name,
                    .{ .symbol = .{ .is_mut = var_def.binding == .let_mut, .type = final_type } },
                    .{
                        .is_defined = false,
                        .inner_name = try self.mangle(var_def.variable_name),
                    },
                );

                try self.exported_symbols.put(var_def.variable_name, .{
                    .name = var_def.variable_name,
                    .inner_name = try self.mangle(var_def.variable_name),
                    .is_pub = var_def.is_pub,
                    .type = final_type,
                    .is_mut = var_def.binding == .let_mut,
                });
            },
            .binding_type_declaration => |btd| {
                const t: Type = switch (btd.type) {
                    .@"struct" => .{ .@"struct" = try Type.Struct.init(self, btd.name, btd.name, null) },
                    .@"union" => .{ .@"union" = try Type.Union.init(self, btd.name, btd.name, null) },
                    .@"enum" => .{ .@"enum" = try Type.Enum.init(self, btd.name, btd.name, null) },
                };

                try self.registerSymbol(btd.name, .{ .type = t }, .{});

                try self.exported_symbols.put(btd.name, .{
                    .name = btd.name,
                    .inner_name = btd.name,
                    .is_pub = btd.is_pub,
                    .type = t,
                });
            },
            inline .struct_declaration, .union_declaration, .enum_declaration => |*struct_decl, t| {
                const tag: utils.CompoundTypeTag = switch (t) {
                    .struct_declaration => .@"struct",
                    .union_declaration => .@"union",
                    .enum_declaration => .@"enum",
                    else => unreachable,
                };
                const compound_type = try Type.fromCompoundTypeDeclaration(self, tag, struct_decl, .{});
                const symbol_type = @unionInit(Type, @tagName(tag), compound_type);

                // Exported symbols logic
                try self.exported_symbols.put(struct_decl.name, .{
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
    // ── Build the path relative to the current source file (existing behaviour) ──
    var parts: std.ArrayList([]const u8) = .empty;
    defer parts.deinit(self.alloc);

    if (std.fs.path.dirname(self.source_path)) |dir| try parts.append(self.alloc, dir);
    for (import_stmt.module_name) |part| try parts.append(self.alloc, part);

    const rel_path = try std.fs.path.join(self.alloc, parts.items);
    defer self.alloc.free(rel_path);

    const full_path = try std.fmt.allocPrint(self.alloc, "{s}.zag", .{rel_path});
    defer self.alloc.free(full_path);

    // ── Registry hit (avoids recompiling the same module twice) ──────────────
    if (self.module_registry.get(full_path)) |mod| return mod;

    // ── Determine the actual path to compile ─────────────────────────────────
    const resolved_path: []const u8 = if (std.fs.cwd().access(full_path, .{}) == error.FileNotFound) blk: {
        // Relative path doesn't exist — try the stdlib install directory.
        var stdlib_parts: std.ArrayList([]const u8) = .empty;
        defer stdlib_parts.deinit(self.alloc);

        try stdlib_parts.append(self.alloc, build_options.stdlib_path);
        for (import_stmt.module_name) |part| try stdlib_parts.append(self.alloc, part);

        const stdlib_rel = try std.fs.path.join(self.alloc, stdlib_parts.items);
        defer self.alloc.free(stdlib_rel);

        const stdlib_full = try std.fmt.allocPrint(self.alloc, "{s}.zag", .{stdlib_rel});
        errdefer self.alloc.free(stdlib_full);

        std.fs.cwd().access(stdlib_full, .{}) catch return utils.printErr(
            error.ModuleNotFound,
            "Module not found: '{s}'\n  searched: {s}\n  searched: {s}\n",
            .{ import_stmt.module_name[import_stmt.module_name.len - 1], full_path, stdlib_full },
            .red,
        );

        break :blk stdlib_full; // caller must free; it lands in module_registry below
    } else full_path;

    // Check the registry again with the resolved path (handles stdlib cache hits)
    if (self.module_registry.get(resolved_path)) |mod| return mod;

    // ── Compile the module (existing logic, unchanged) ────────────────────────
    const ast_res = try getAST(self.alloc, resolved_path);
    defer self.alloc.free(ast_res.source);

    var child_compiler = try init(self.alloc, ast_res.root, resolved_path, self.module_registry);
    defer child_compiler.deinit();

    try child_compiler.analyze();

    var mod: Module = try .init(self.alloc, import_stmt.alias orelse import_stmt.module_name[import_stmt.module_name.len - 1]);

    var it = child_compiler.exported_symbols.iterator();
    while (it.next()) |entry| {
        switch (entry.value_ptr.type) {
            inline .@"struct", .@"union", .function => |*s| s.module = mod,
            else => {},
        }
        try mod.symbols.put(entry.key_ptr.*, entry.value_ptr.*);
    }

    var imp_it = child_compiler.imported_modules.iterator();
    while (imp_it.next()) |entry| try mod.imports.put(entry.key_ptr.*, entry.value_ptr.*);

    const owned_path = try self.alloc.dupe(u8, resolved_path);
    errdefer self.alloc.free(owned_path);

    const gop = try self.module_registry.getOrPut(owned_path);
    if (gop.found_existing) {
        self.alloc.free(owned_path); // don't need it, registry already has a copy
        return gop.value_ptr.*;
    }
    gop.value_ptr.* = mod;

    return mod;
}

pub fn compileBlock(
    self: *Self,
    block: ast.Block,
    opts: struct {
        capture: ?struct {
            condition: *const ast.Expression,
            capture: utils.Capture,
        } = null,
        iterator: ?struct {
            iter_expr: *const ast.Expression,
            index: []const u8,
            capture: utils.Capture,
        } = null,
        return_type_override: ?Type = null,
        return_type_override_is_array: bool = false,
        return_implicit_success: ?Type = null,
        is_loop_body: bool = false,
    },
) CompilerError!void {
    try self.pushScope(opts.is_loop_body);
    defer self.popScope();

    try self.write("{\n");

    if (opts.capture) |capture| {
        const condition_type: Type = try .infer(self, capture.condition.*);
        const inner_type: Type = if (capture.capture.takes_ref == .some) .{
            .reference = .{
                .is_mut = capture.capture.takes_ref.some,
                .inner = condition_type.optional,
            },
        } else condition_type.optional.*;

        try self.compileVariableSignature(capture.capture.name, inner_type, .let);
        try self.print(" = {s}(", .{if (capture.capture.takes_ref == .some) "&" else ""});
        try expressions.compile(self, capture.condition, .{});
        try self.write(").payload;\n");

        try self.registerSymbol(capture.capture.name, .{ .symbol = .{ .type = inner_type } }, .{});
    }

    if (opts.iterator) |iterator| {
        const t: Type = try .infer(self, iterator.iter_expr.*);
        const inner_type: Type = if (iterator.capture.takes_ref == .some) .{
            .reference = .{
                .is_mut = iterator.capture.takes_ref.some,
                .inner = switch (t) {
                    inline .array, .slice => |ti| ti.inner,
                    else => unreachable,
                },
            },
        } else switch (t) {
            inline .array, .slice => |ti| ti.inner.*,
            else => unreachable,
        };

        try self.compileVariableSignature(iterator.capture.name, inner_type, .let);
        try self.print(" = {s}(", .{if (iterator.capture.takes_ref == .some) "&" else ""});
        try expressions.compile(self, iterator.iter_expr, .{});
        try self.write(")");
        if (t == .slice) try self.write(".ptr");
        try self.print("[{s}];\n", .{iterator.index});

        try self.registerSymbol(iterator.capture.name, .{ .symbol = .{ .type = inner_type } }, .{});
    }

    var returns: union(enum) { yes: ?ast.Expression, no, @"break", @"continue" } = .no;
    for (block) |*statement| switch (statement.*) {
        .@"return" => |r| {
            returns = .{ .yes = r.@"return" };
            break;
        },
        .block_eval => |be| {
            if (try Type.infer(self, be) != .void)
                returns = .{ .yes = be }
            else
                try statements.compile(self, statement);
            break;
        },
        .@"continue" => {
            returns = .@"continue";
            break;
        },
        .@"break" => {
            returns = .@"break";
            break;
        },
        else => try statements.compile(self, statement),
    };

    switch (returns) {
        .yes => |return_expr| if (return_expr) |*e| {
            if (self.current_return_type.? == .void) return utils.printErr(
                error.IllegalExpression,
                "comperr: Cannot return value in a void function ({f}).\n",
                .{e.getPosition()},
                .red,
            );

            const return_type = opts.return_type_override orelse self.current_return_type.?;
            self.currentSection().current_statement = self.currentWriter().items.len;
            try self.compileVariableSignature("__zag_ret_val", return_type, .let);
            try self.write(" = ");
            if (opts.return_type_override_is_array) try self.print("({s}){{ .items = ", .{opts.return_type_override.?.@"struct".inner_name});
            try expressions.compile(self, e, .{
                .is_variable_declaration = true,
                .expected_type = self.current_return_type.?,
            });
            if (opts.return_type_override_is_array) try self.write("}");
            try self.write(";\n");
        },
        else => {},
    }

    switch (returns) {
        .yes => |expr| {
            var it = std.mem.reverseIterator(self.scopes.items);
            while (it.next()) |scope| {
                for (scope.pending_defers.items) |pd| try statements.compile(self, pd);
                if (scope.is_function_boundary) break;
            }

            if (expr) |_| {
                self.currentSection().current_statement = self.currentWriter().items.len;
                try self.write("return __zag_ret_val;");
            } else try self.write("return;\n");
        },
        .no => {
            for (self.scopes.getLast().pending_defers.items) |pd| try statements.compile(self, pd);
            if (opts.return_implicit_success) |ris| {
                self.currentSection().current_statement = self.currentWriter().items.len;
                try self.write("return (");
                try self.compileType(ris, .{});
                try self.write("){ .is_success = true, .payload.success = 0 };\n");
            }
        },
        inline .@"break", .@"continue" => |_, t| {
            var it = std.mem.reverseIterator(self.scopes.items);
            while (it.next()) |scope| {
                for (scope.pending_defers.items) |pd| try statements.compile(self, pd);
                if (scope.is_loop_body) break;
            }

            try self.print("{s};\n", .{@tagName(t)});
        },
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
        inline .@"struct", .@"union", .@"enum" => |s| try self.write(s.inner_name),
        .optional, .slice, .error_union, .array, .function => try self.write(try self.getTypeFromZagHeader(t)),
        .variadic => try self.write("..."),
        .any => try self.write("void*"),
        else => |primitive| try self.write(try self.getInnerName(@tagName(primitive))),
    };
}

pub fn compileVariableSignature(
    self: *Self,
    name: []const u8,
    t: Type,
    binding: utils.Binding,
) CompilerError!void {
    if (t == .type) unreachable;

    if (binding == .@"const") try self.write("static ");

    const binding_mut = binding == .let_mut or t == .slice and t.slice.is_mut;

    switch (t) {
        .array => |array| {
            try self.compileType(array.inner.*, .{ .binding_mut = binding_mut });
            try self.print(" {s}[{}]", .{ name, array.size });
        },
        .function => |function| {
            if (self.zag_header_contents.get(t)) |type_name| {
                try self.print("{s} {s}", .{ type_name, name });
                return;
            }

            try self.compileType(function.return_type.*, .{ .binding_mut = true });
            try self.print(" (*{s})(", .{name});
            for (function.params, 0..) |param, i| {
                try self.compileType(param.type, .{ .binding_mut = binding == .let_mut });
                if (i < function.params.len - 1) try self.write(", ");
            }
            try self.write(")");
        },
        .variadic => try self.write("..."),
        else => {
            try self.compileType(t, .{ .binding_mut = binding_mut });
            try self.print(" {s}", .{name});
        },
    }
}

pub fn solveComptimeExpression(self: *Self, expression: ast.Expression) !Value {
    const original_result: Value = switch (expression) {
        .int => |int| .{ .u64 = int.payload },
        .float => |float| .{ .f64 = float.payload },
        .char => |char| .{ .u8 = char.payload },
        .binary => |binary| {
            const lhs = try self.solveComptimeExpression(binary.lhs.*);
            defer lhs.deinit(self.alloc);
            const rhs = try self.solveComptimeExpression(binary.rhs.*);
            defer rhs.deinit(self.alloc);
            return try lhs.binaryOperation(binary.op, rhs);
        },
        .ident => |ident| b: {
            const item = self.getScopeItem(ident.payload) catch |err| switch (err) {
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
                .symbol => |s| switch (s.type) {
                    .function => |f| .{ .function = f },
                    .type_type => .{ .type = .type_type },
                    .type => |t| .{ .type = t.* },
                    else => return error.ExpressionCannotBeEvaluatedAtCompileTime,
                },
            };
        },
        .generic => .{ .type = try .infer(self, expression) },
        .type => |t| .{ .type = try .fromAst(self, t.payload) },
        .prefix => |prefix| {
            const rhs = try self.solveComptimeExpression(prefix.rhs.*);
            defer rhs.deinit(self.alloc);
            return switch (prefix.op) {
                .@"-" => switch (rhs) {
                    inline .i64, .u64, .f64 => |number, tag| if (tag == .u64)
                        .{ .i64 = -@as(i64, @intCast(number)) }
                    else if (tag == .i64)
                        .{ .i64 = -number }
                    else if (tag == .f64)
                        .{ .f64 = -number }
                    else
                        unreachable,
                    else => |other| return errors.illegalPrefixExpression(prefix.op, other.getType(), expression.getPosition()),
                },
                .@"!" => switch (rhs) {
                    .bool => |b| .{ .bool = !b },
                    else => |other| return errors.illegalPrefixExpression(prefix.op, other.getType(), expression.getPosition()),
                },
            };
        },
        .struct_instantiation => |inst| .{
            .comptime_struct = .{
                .type = try Type.infer(self, expression),
                .fields = b: {
                    const fields = try self.alloc.alloc(Value.ComptimeStruct.Field, inst.members.count());
                    var i: usize = 0;
                    var it = inst.members.iterator();
                    while (it.next()) |member| : (i += 1) fields[i] = .{
                        .name = member.key_ptr.*,
                        .value = self.solveComptimeExpression(member.value_ptr.*) catch
                            return error.ExpressionCannotBeEvaluatedAtCompileTime,
                    };
                    break :b fields;
                },
            },
        },
        .member => |member_expr| {
            const parent_val = self.solveComptimeExpression(member_expr.parent.*) catch
                return error.ExpressionCannotBeEvaluatedAtCompileTime;
            defer parent_val.deinit(self.alloc);
            return switch (parent_val) {
                .comptime_struct => |cs| {
                    for (cs.fields) |field| {
                        if (std.mem.eql(u8, field.name, member_expr.member_name)) {
                            return try field.value.clone(self.alloc);
                        }
                    }
                    return error.ExpressionCannotBeEvaluatedAtCompileTime;
                },
                .type => .{ .type = try .infer(self, expression) },
                else => {
                    std.debug.print("cannot access member of comptime value: {f}\n", .{parent_val});
                    return error.ExpressionCannotBeEvaluatedAtCompileTime;
                },
            };
        },
        .@"if" => |expr| {
            const cond_val = try self.solveComptimeExpression(expr.condition.*);
            defer cond_val.deinit(self.alloc);
            return switch (cond_val) {
                .bool => |cond| if (cond)
                    try self.solveComptimeExpression(expr.body.*)
                else if (expr.@"else") |else_branch|
                    try self.solveComptimeExpression(else_branch.*)
                else
                    return errors.ifExpressionMustContainElseClause(expr.pos),
                else => return error.ExpressionCannotBeEvaluatedAtCompileTime,
            };
        },
        else => return error.ExpressionCannotBeEvaluatedAtCompileTime,
    };

    return if (original_result == .type)
        .{ .type = try original_result.type.clone(self.alloc) }
    else if (original_result == .comptime_struct) .{
        .comptime_struct = .{
            .type = try original_result.comptime_struct.type.clone(self.alloc),
            .fields = try utils.cloneSlice(Value.ComptimeStruct.Field, original_result.comptime_struct.fields, self.alloc),
        },
    } else original_result;
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

        try self.pushScope(false);
        defer self.popScope();

        var it = instantiation.module.symbols.iterator();
        while (it.next()) |entry| {
            const sym = entry.value_ptr.*;
            const scope_item: ScopeItem = switch (sym.type) {
                .@"enum", .@"struct", .@"union" => .{
                    .type = .{
                        .type = sym.type,
                        .inner_name = sym.inner_name,
                        .is_defined = true,
                        .should_free = false,
                    },
                },
                else => .{
                    .symbol = .{
                        .type = sym.type,
                        .inner_name = sym.inner_name,
                        .is_mut = false,
                        .is_defined = true,
                        .should_free = false,
                    },
                },
            };
            try self.scopes.getLast().items.put(entry.key_ptr.*, scope_item);
        }

        var imp_it = instantiation.module.imports.iterator();
        while (imp_it.next()) |entry|
            try self.registerSymbol(entry.key_ptr.*, .{ .module = entry.value_ptr.* }, .{});

        switch (instantiation.t) {
            inline .@"struct", .@"union" => |s, tag| {
                for (s.generic_types, 0..) |param, j| {
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
                        .generic_types = &.{},
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
                for (f.generic_parameters, 0..) |param, j| {
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
                        .generic_parameters = &.{},
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
pub fn pushScope(self: *Self, is_loop_body: bool) !void {
    const items = try self.alloc.create(std.StringHashMap(ScopeItem));
    items.* = .init(self.alloc);

    const pending_defers = try self.alloc.create(std.ArrayList(*const ast.Statement));
    pending_defers.* = .empty;

    try self.scopes.append(self.alloc, .{
        .pending_defers = pending_defers,
        .items = items,
        .is_loop_body = is_loop_body,
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
        should_free: bool = false,
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
                .should_free = opts.should_free,
            },
        },
        .type => |@"type"| .{
            .type = .{
                .type = @"type",
                .is_defined = opts.is_defined,
                .inner_name = opts.inner_name orelse name,
                .should_free = opts.should_free,
            },
        },
        .module => |module| .{ .module = module },
        .constant => |constant| .{
            .constant = .{
                .type = constant.type,
                .value = constant.value,
                .inner_name = opts.inner_name orelse name,
                .should_free = opts.should_free,
            },
        },
    });
}

/// Registers boolean constants, null, undefined, and primitive types
fn registerConstants(self: *Self) !void {
    try self.registerSymbol("true", .{ .symbol = .{ .type = .bool } }, .{});
    try self.registerSymbol("false", .{ .symbol = .{ .type = .bool } }, .{});
    try self.registerSymbol("nil", .{ .symbol = .{ .type = .@"typeof(nil)" } }, .{});
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

    try self.registerSymbol("type", .{ .type = .type_type }, .{});

    try self.registerSymbol("c_int", .{ .type = .c_int }, .{ .inner_name = "int" });
    try self.registerSymbol("c_char", .{ .type = .c_char }, .{ .inner_name = "char" });
    try self.registerSymbol("c_long", .{ .type = .c_long }, .{ .inner_name = "long" });
    try self.registerSymbol("c_short", .{ .type = .c_short }, .{ .inner_name = "short" });

    try self.registerSymbol("c_uint", .{ .type = .c_uint }, .{ .inner_name = "unsigned int" });
    try self.registerSymbol("c_uchar", .{ .type = .c_uchar }, .{ .inner_name = "unsigned char" });
    try self.registerSymbol("c_ulong", .{ .type = .c_ulong }, .{ .inner_name = "unsigned long" });
    try self.registerSymbol("c_ushort", .{ .type = .c_ushort }, .{ .inner_name = "unsigned short" });

    try self.registerSymbol("c_float", .{ .type = .c_float }, .{ .inner_name = "float" });
    try self.registerSymbol("c_double", .{ .type = .c_double }, .{ .inner_name = "double" });

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
                        const params = try self.alloc.alloc(Type.Function.Param, 1);
                        params[0] = .{ .name = "T", .type = .type_type };
                        break :b params;
                    },
                    .return_type = b: {
                        const t = try self.alloc.create(Type);
                        t.* = .usize;
                        break :b t;
                    },
                    .params = &.{},
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
                        const params = try self.alloc.alloc(Type.Function.Param, 1);
                        params[0] = .{ .name = "T", .type = .type_type };
                        break :b params;
                    },
                    .return_type = b: {
                        const t = try self.alloc.create(Type);
                        t.* = .usize; // Default, will be overridden
                        break :b t;
                    },
                    .params = b: {
                        const params = try self.alloc.alloc(Type.Function.Param, 1);
                        params[0] = .{ .name = "val", .type = .any };
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
                        const params = try self.alloc.alloc(Type.Function.Param, 1);
                        params[0] = .{ .name = "T", .type = .type_type };
                        break :b params;
                    },
                    .return_type = b: {
                        const t = try self.alloc.create(Type);
                        t.* = .usize; // Default, will be overridden
                        break :b t;
                    },
                    .params = b: {
                        const params = try self.alloc.alloc(Type.Function.Param, 2);
                        params[0] = .{ .name = "a", .type = .any };
                        params[1] = .{ .name = "b", .type = .any };
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
        else => return utils.printErr(
            error.SymbolNotVariable,
            "Compiler error: Symbol '{s}' is not a variable.\n",
            .{symbol},
            .red,
        ),
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

pub fn getExpressionMutability(self: *Self, expr: ast.Expression) !bool {
    return switch (expr) {
        .ident => |ident| self.getSymbolMutability(ident.payload),
        .member => |m| {
            const parent_type = try Type.infer(self, m.parent.*);
            return switch (parent_type) {
                .reference => |ref| ref.is_mut,
                .module => |mod| {
                    const symbol = mod.symbols.get(m.member_name) orelse return false;
                    return symbol.is_mut;
                },
                inline .@"struct", .@"union", .slice => self.getExpressionMutability(m.parent.*),
                else => false,
            };
        },
        inline .index, .slice => |idx| {
            const lhs_type = try Type.infer(self, idx.lhs.*);
            return switch (lhs_type) {
                .slice => |slc| slc.is_mut,
                .array => self.getExpressionMutability(idx.lhs.*),
                else => false,
            };
        },
        .dereference => |deref| {
            const t = try Type.infer(self, deref.parent.*);
            return t == .reference and t.reference.is_mut;
        },
        else => false,
    };
}

pub fn getInnerName(self: *const Self, symbol: []const u8) ![]const u8 {
    var it = std.mem.reverseIterator(self.scopes.items);
    while (it.next()) |scope| {
        if (scope.items.get(symbol)) |item| {
            return switch (item) {
                .module => |module| module.name,
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

pub fn getTypeFromZagHeader(self: *Self, t: Type) ![]const u8 {
    var type_name: []const u8 = undefined;
    switch (t) {
        .optional => |optional| {
            type_name = try std.fmt.allocPrint(self.alloc, "__zag_Optional_{x}", .{t.hash()});
            if (self.zag_header_contents.get(t) == null) {
                try self.beginTypeDefEmit(t);

                try self.write("typedef struct {\n");
                try self.compileVariableSignature("is_some", .bool, .let_mut);
                try self.write(";\n");
                try self.compileVariableSignature("payload", optional.*, .let_mut);
                try self.write(";\n");
                try self.print("}} {s};\n", .{type_name});

                self.endTypeDefEmit();
                try self.zag_header_contents.put(t, type_name);
            }
        },
        .error_union => |error_union| {
            type_name = try std.fmt.allocPrint(self.alloc, "__zag_ErrorUnion_{x}", .{t.hash()});
            if (self.zag_header_contents.get(t) == null) {
                try self.beginTypeDefEmit(t);

                try self.write("typedef struct {\n");
                try self.write("bool is_success;\n");
                try self.write("union {\n");
                // if the success type is void, use u8 as a small dummy type since C union members can't be void
                try self.compileVariableSignature(
                    "success",
                    if (error_union.success.* == .void) .u8 else error_union.success.*,
                    .let_mut,
                );
                try self.write(";\n");
                try self.compileVariableSignature("failure", error_union.failure.*, .let_mut);
                try self.print(";\n}} payload;\n}} {s};\n", .{type_name});

                self.endTypeDefEmit();
                try self.zag_header_contents.put(t, type_name);
            }
        },
        .slice => |slice| {
            // replace mutability with true. this is done purely for c codegen ease regarding
            // the fact that `[]T` and `[]mut T` generate different hashes, but should be compatible
            // with each other
            const canonical: Type = .{ .slice = .{ .inner = slice.inner, .is_mut = true } };
            type_name = try std.fmt.allocPrint(self.alloc, "__zag_Slice_{x}", .{canonical.hash()});
            if (self.zag_header_contents.get(canonical) == null) {
                try self.beginTypeDefEmit(canonical);

                try self.write("typedef struct {\n");
                try self.compileVariableSignature("ptr", .{
                    .reference = .{
                        .is_mut = true,
                        .inner = slice.inner,
                    },
                }, .let_mut);
                try self.write(";\n");
                try self.compileVariableSignature("len", .usize, .let_mut);
                try self.print(";\n}} {s};\n", .{type_name});

                self.endTypeDefEmit();
                try self.zag_header_contents.put(canonical, type_name);
            }
        },
        .array => |array| {
            type_name = try std.fmt.allocPrint(self.alloc, "__zag_{x}", .{t.hash()});
            if (self.zag_header_contents.get(t) == null) {
                try self.beginTypeDefEmit(t);
                try self.write("typedef struct { ");
                try self.compileType(array.inner.*, .{ .binding_mut = true, .is_top_level = false });
                try self.print(" v[{}]; }} {s};\n", .{ array.size, type_name });
                self.endTypeDefEmit();
                try self.zag_header_contents.put(t, type_name);
            }
        },
        .function => |function| {
            type_name = try std.fmt.allocPrint(self.alloc, "__zag_FuncPtr_{x}", .{t.hash()});
            if (self.zag_header_contents.get(t) == null) {
                try self.beginTypeDefEmit(t);

                try self.write("typedef ");
                try self.compileType(function.return_type.*, .{});
                try self.print(" (*{s})(", .{type_name});
                for (function.params, 0..) |param, i| {
                    try self.compileType(param.type, .{});
                    if (i < function.params.len - 1) try self.write(", ");
                }
                try self.write(");\n");

                self.endTypeDefEmit();
                try self.zag_header_contents.put(t, type_name);
            }
        },
        else => unreachable,
    }

    return type_name;
}

pub fn getScopeItemWithinFunction(self: *const Self, symbol: []const u8) !ScopeItem {
    var it = std.mem.reverseIterator(self.scopes.items);
    while (it.next()) |scope| {
        if (scope.items.get(symbol)) |item| return item;
        if (scope.is_function_boundary) break; // stop at function boundary
    }
    return error.UnknownSymbol;
}

pub fn collectTypeDeps(self: *Self, t: Type, out: *std.ArrayList(Type)) !void {
    switch (t) {
        inline .@"struct", .@"union" => |s| {
            var it = s.members.iterator();
            while (it.next()) |e| try self.appendIfConcrete(e.value_ptr.*, out);
        },
        .error_union => |eu| {
            try self.appendIfConcrete(eu.success.*, out);
            try self.appendIfConcrete(eu.failure.*, out);
        },
        .optional => |inner| try self.appendIfConcrete(inner.*, out),
        .array => |array| try self.appendIfConcrete(array.inner.*, out),
        .slice => |slice| try self.appendIfConcrete(slice.inner.*, out),
        else => {},
    }
}

pub fn emitTypeDefsInOrder(self: *Self, writer: anytype) !void {
    const n = self.type_def_blocks.items.len;
    const visited = try self.alloc.alloc(u8, n);
    defer self.alloc.free(visited);
    @memset(visited, 0);

    var order: std.ArrayList(usize) = .empty;
    defer order.deinit(self.alloc);

    for (0..n) |i| if (visited[i] == 0) try analysis.topoVisit(self, i, visited, &order);

    for (order.items) |idx| {
        const block = &self.type_def_blocks.items[idx];
        const guard = try std.fmt.allocPrint(self.alloc, "#ifndef __ZAG_TYPE_{x}\n#define __ZAG_TYPE_{0x}\n", .{block.type.hash()});
        defer self.alloc.free(guard);
        try writer.writeAll(guard);
        try writer.writeAll(block.code.items);
        try writer.writeAll("#endif\n");
    }
}

fn appendIfConcrete(self: *Self, t: Type, out: *std.ArrayList(Type)) !void {
    switch (t) {
        .@"struct", .@"union", .@"enum", .error_union, .slice, .optional, .array => try out.append(self.alloc, t),
        else => {},
    }
}

pub fn beginTypeDefEmit(self: *Self, t: Type) !void {
    const idx = self.type_def_blocks.items.len;
    try self.type_def_blocks.append(self.alloc, .{
        .type = t,
        .code = .empty,
        .saved_section = self.current_section,
    });
    try self.type_def_stack.append(self.alloc, idx);
    self.switchSection(.header_type_defs);
}

pub fn endTypeDefEmit(self: *Self) void {
    const idx = self.type_def_stack.pop() orelse {
        std.debug.print("popping failed, returning early\n", .{});
        return;
    };
    self.switchSection(self.type_def_blocks.items[idx].saved_section);
}
