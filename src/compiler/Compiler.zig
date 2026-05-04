const std = @import("std");
const ast = @import("ast");
const utils = @import("utils");
const build_options = @import("build_options");

const parser = @import("parser");
const lexer = @import("lexer");

const errors = @import("errors.zig");
const Error = errors.Error;
const statements = @import("statements.zig");

pub const Type = @import("type.zig").Type;
pub const Module = @import("Module.zig");
pub const Value = @import("value.zig").Value;

pub const Symbol = struct {
    name: []const u8,
    inner_name: []const u8,
    type: Type,
    binding: utils.Binding = .let,
    is_pub: bool = false,
    value: ?Value = null,
    free_type: bool,
    free_name: bool,
    free_inner_name: bool,

    pub fn deinit(self: Symbol, alloc: std.mem.Allocator) void {
        if (self.free_type) self.type.deinit(alloc);
        if (self.free_name) alloc.free(self.name);
        if (self.free_inner_name) alloc.free(self.inner_name);
        if (self.value) |v| v.deinit(alloc);
    }

    pub fn eql(lhs: Symbol, rhs: Symbol) bool {
        return std.mem.eql(u8, lhs.name, rhs.name) and
            std.mem.eql(u8, lhs.inner_name, rhs.inner_name) and
            lhs.type.eql(rhs.type) and
            lhs.binding == rhs.binding and
            lhs.is_pub == rhs.is_pub and
            (lhs.value == null and rhs.value == null or
                (lhs.value != null and rhs.value != null and lhs.value.?.eql(rhs.value.?)));
    }

    pub fn clone(self: Symbol, alloc: std.mem.Allocator) Error!Symbol {
        const name = if (self.free_name) try alloc.dupe(u8, self.name) else self.name;
        errdefer if (self.free_name) alloc.free(name);

        const inner_name = if (self.free_inner_name) try alloc.dupe(u8, self.inner_name) else self.inner_name;
        errdefer if (self.free_inner_name) alloc.free(inner_name);

        const t = if (self.free_type) try self.type.clone(alloc) else self.type;
        errdefer if (self.free_type) t.deinit(alloc);

        const value = if (self.value) |v| try v.clone(alloc) else null;
        errdefer if (value) |v| v.deinit(alloc);

        return .{
            .name = name,
            .inner_name = inner_name,
            .type = t,
            .binding = self.binding,
            .is_pub = self.is_pub,
            .value = value,
            .free_type = self.free_type,
            .free_name = self.free_name,
            .free_inner_name = self.free_inner_name,
        };
    }
};

pub const Compiler = struct {
    source: struct {
        includes: *std.ArrayList(u8),
        variables: *std.ArrayList(u8),
        function_impls: *std.ArrayList(u8),

        fn init(alloc: std.mem.Allocator) !@This() {
            const self: @This() = .{
                .includes = try alloc.create(std.ArrayList(u8)),
                .variables = try alloc.create(std.ArrayList(u8)),
                .function_impls = try alloc.create(std.ArrayList(u8)),
            };
            self.includes.* = .empty;
            self.variables.* = .empty;
            self.function_impls.* = .empty;
            return self;
        }

        fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
            self.includes.deinit(alloc);
            self.variables.deinit(alloc);
            self.function_impls.deinit(alloc);

            alloc.destroy(self.includes);
            alloc.destroy(self.variables);
            alloc.destroy(self.function_impls);
        }

        fn write(self: *@This(), writer: *std.Io.Writer) !void {
            try writer.writeAll(self.includes.items);
            try writer.writeAll(self.variables.items);
            try writer.writeAll(self.function_impls.items);
            try writer.flush();
        }
    },

    header: struct {
        includes: *std.ArrayList(u8),
        typedefs: *std.ArrayList(u8),
        forward_decls: *std.ArrayList(u8),
        variables: *std.ArrayList(u8),
        function_decls: *std.ArrayList(u8),

        fn init(alloc: std.mem.Allocator) !@This() {
            const self: @This() = .{
                .includes = try alloc.create(std.ArrayList(u8)),
                .typedefs = try alloc.create(std.ArrayList(u8)),
                .variables = try alloc.create(std.ArrayList(u8)),
                .forward_decls = try alloc.create(std.ArrayList(u8)),
                .function_decls = try alloc.create(std.ArrayList(u8)),
            };
            self.includes.* = .empty;
            self.typedefs.* = .empty;
            self.variables.* = .empty;
            self.forward_decls.* = .empty;
            self.function_decls.* = .empty;
            return self;
        }

        fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
            self.includes.deinit(alloc);
            self.typedefs.deinit(alloc);
            self.variables.deinit(alloc);
            self.forward_decls.deinit(alloc);
            self.function_decls.deinit(alloc);

            alloc.destroy(self.includes);
            alloc.destroy(self.typedefs);
            alloc.destroy(self.variables);
            alloc.destroy(self.forward_decls);
            alloc.destroy(self.function_decls);
        }

        fn write(self: *@This(), writer: *std.Io.Writer) !void {
            try writer.writeAll(self.includes.items);
            try writer.writeAll(self.typedefs.items);
            try writer.writeAll(self.variables.items);
            try writer.writeAll(self.forward_decls.items);
            try writer.writeAll(self.function_decls.items);
            try writer.flush();
        }
    },

    primitives: std.BufSet,
    module: *Module,

    /// borrowed
    module_registry: *std.StringHashMap(*Module),

    fn deinit(self: *Compiler, alloc: std.mem.Allocator) void {
        self.module.deinit(alloc);
        self.deinitWithoutModule(alloc);
    }

    fn deinitWithoutModule(self: *Compiler, alloc: std.mem.Allocator) void {
        self.source.deinit(alloc);
        self.header.deinit(alloc);
        self.primitives.deinit();
    }

    /// Caller owns memory
    pub fn compileType(
        self: *Compiler,
        alloc: std.mem.Allocator,
        io: std.Io,
        t: *const Type,
        m: *Module,
        pos: usize,
    ) ![]const u8 {
        return switch (t.*) {
            .optional => |optional| {
                const type_name = try std.fmt.allocPrint(alloc, "__zag_Optional_{x}", .{optional.hash()});
                if (!self.primitives.contains(type_name)) {
                    const t_comp = try self.compileType(alloc, io, optional, m, pos);
                    defer alloc.free(t_comp);
                    try self.header.typedefs.print(
                        alloc,
                        "typedef struct {s} {{ bool is_some; {s} payload; }} {0s};",
                        .{ type_name, t_comp },
                    );

                    try self.primitives.insert(type_name);
                }

                return type_name;
            },
            .reference => |ref| {
                const inner = try self.compileType(alloc, io, ref.inner, m, pos);
                defer alloc.free(inner);
                return try std.fmt.allocPrint(alloc, "{s}{s}*", .{
                    if (!ref.is_mut) "const " else "",
                    inner,
                });
            },
            .variadic => unreachable,
            .@"struct", .@"union", .@"enum" => {
                const symbol = m.findSymbolByType(t.*) orelse self.module.findSymbolByType(t.*).?;
                return try alloc.dupe(u8, symbol.inner_name);
            },
            .array => |array| {
                const type_name = try std.fmt.allocPrint(alloc, "__zag_Array_{x}", .{t.hash()});
                if (!self.primitives.contains(type_name)) {
                    const t_comp = try self.compileType(alloc, io, array.inner, m, pos);
                    defer alloc.free(t_comp);
                    try self.header.typedefs.print(
                        alloc,
                        "typedef struct {s} {{ {s} items[{}]; }} {0s};",
                        .{ type_name, t_comp, array.len },
                    );

                    try self.primitives.insert(type_name);
                }

                return type_name;
            },
            .slice => |slice| {
                const type_name = try std.fmt.allocPrint(alloc, "__zag_Slice_{x}", .{t.hash()});
                if (!self.primitives.contains(type_name)) {
                    const t_comp = try self.compileType(alloc, io, slice.inner, m, pos);
                    defer alloc.free(t_comp);
                    try self.header.typedefs.print(
                        alloc,
                        "typedef struct {s} {{ {s}{s} *ptr; size_t len; }} {0s};",
                        .{ type_name, if (slice.is_mut) "" else "const ", t_comp },
                    );

                    try self.primitives.insert(type_name);
                }

                return type_name;
            },
            inline else => |_, tag| if (self.module.getSymbol(@tagName(tag))) |symbol|
                alloc.dupe(u8, symbol.inner_name)
            else
                errors.unknownSymbol(io, @tagName(tag), m.source_map[pos]),
        };
    }

    fn analyze(
        c: *Compiler,
        alloc: std.mem.Allocator,
        io: std.Io,
        root_node: []const ast.TopLevelStatement,
        m: *Module,
    ) !void {
        for (root_node) |statement| switch (statement) {
            .binding_function_declaration => |bfd| {
                const function_type_ast = try bfd.getType(alloc);
                defer function_type_ast.deinit(alloc);

                const function_type: Type = try .fromAst(alloc, io, &function_type_ast, c, m);
                const symbol: Symbol = .{
                    .name = bfd.name,
                    .inner_name = bfd.name,
                    .type = function_type,
                    .binding = .@"const",
                    .is_pub = bfd.is_pub,
                    .free_name = false,
                    .free_inner_name = false,
                    .free_type = true,
                };
                errdefer symbol.deinit(alloc);
                try c.module.register(alloc, symbol);
            },
            .binding_type_declaration => |btd| {
                const symbol: Symbol = .{
                    .name = btd.name,
                    .inner_name = btd.name,
                    .type = .type,
                    .binding = .@"const",
                    .is_pub = btd.is_pub,
                    .value = .{
                        .type = switch (btd.type) {
                            inline else => |tag| @unionInit(Type, @tagName(tag), .{
                                .name = btd.name,
                                .inner_name = btd.name,
                                .members = &.{},
                                .symbols = &.{},
                                .tag_type = undefined, // todo: binding unions might need a specified tag_type
                            }),
                        },
                    },
                    .free_name = false,
                    .free_inner_name = false,
                    .free_type = false,
                };
                errdefer symbol.deinit(alloc);
                try c.module.register(alloc, symbol);
            },
            .function_definition => |fd| {
                if (fd.generic_parameters.len > 0) {
                    const symbol: Symbol = .{
                        .name = fd.name,
                        .inner_name = fd.name,
                        .type = .{ .template = .{
                            .kind = .{ .function_definition = try fd.clone(alloc) },
                            .module = m,
                        } },
                        .binding = .@"const",
                        .is_pub = fd.is_pub,
                        .free_name = false,
                        .free_type = true,
                        .free_inner_name = false,
                    };
                    errdefer symbol.deinit(alloc);
                    try c.module.register(alloc, symbol);
                    continue;
                }

                const inner_name = try std.fmt.allocPrint(alloc, "{s}_{s}", .{ c.module.name, fd.name });
                const function_type_ast = try fd.getType(alloc);
                defer function_type_ast.deinit(alloc);

                const function_type: Type = try .fromAst(alloc, io, &function_type_ast, c, m);
                const symbol: Symbol = .{
                    .name = fd.name,
                    .inner_name = inner_name,
                    .type = function_type,
                    .binding = .@"const",
                    .is_pub = fd.is_pub,
                    .free_name = false,
                    .free_inner_name = true,
                    .free_type = true,
                };
                errdefer symbol.deinit(alloc);
                try c.module.register(alloc, symbol);
            },
            .variable_definition => |vd| {
                const t: Type = if (vd.type) |*t|
                    try .fromAst(alloc, io, t, c, m)
                else
                    try .infer(alloc, io, &vd.assigned_value, c, m);

                const symbol: Symbol = .{
                    .name = vd.variable_name,
                    .type = t,
                    .binding = vd.binding,
                    .inner_name = vd.variable_name,
                    .free_name = false,
                    .free_inner_name = false,
                    .free_type = true,
                };
                errdefer symbol.deinit(alloc);
                try c.module.register(alloc, symbol);
            },
            inline .struct_declaration, .enum_declaration, .union_declaration => |sd, tag| {
                if (tag != .enum_declaration and sd.generic_types.len > 0) {
                    const symbol: Symbol = .{
                        .name = sd.name,
                        .inner_name = sd.name,
                        .type = .{
                            .template = .{
                                .kind = @unionInit(
                                    Type.Template.Kind,
                                    @tagName(tag),
                                    try sd.clone(alloc),
                                ),
                                .module = m,
                            },
                        },
                        .binding = .@"const",
                        .is_pub = sd.is_pub,
                        .free_name = false,
                        .free_type = true,
                        .free_inner_name = false,
                    };
                    errdefer symbol.deinit(alloc);
                    try c.module.register(alloc, symbol);
                    continue;
                }

                var members_set: std.StringHashMap(usize) = .init(alloc);
                defer members_set.deinit();
                for (sd.members) |member| {
                    if (members_set.get(member.name)) |pos|
                        return errors.duplicateStructMember(io, member.name, m.source_map[pos], m.source_map[member.pos]);
                    try members_set.put(member.name, member.pos);
                }
                for (sd.variables) |vd| {
                    if (members_set.get(vd.variable_name)) |pos|
                        return errors.duplicateStructMember(io, vd.variable_name, m.source_map[pos], m.source_map[vd.pos]);
                    try members_set.put(vd.variable_name, vd.pos);
                }
                for (sd.methods) |method| {
                    if (members_set.get(method.name)) |pos|
                        return errors.duplicateStructMember(io, method.name, m.source_map[pos], m.source_map[method.pos]);
                    try members_set.put(method.name, method.pos);
                }

                const t_tag = switch (tag) {
                    .struct_declaration => .@"struct",
                    .union_declaration => .@"union",
                    .enum_declaration => .@"enum",
                    else => unreachable,
                };

                var tag_type: ?*Type = null;
                if (tag == .union_declaration) {
                    tag_type = try alloc.create(Type);
                    tag_type.?.* = .smallestIntegerFor(sd.members.len);
                }

                const inner_name = try std.fmt.allocPrint(alloc, "{s}_{s}", .{ c.module.name, sd.name });

                const symbol = try alloc.create(Symbol);
                symbol.* = .{
                    .name = try alloc.dupe(u8, sd.name),
                    .inner_name = inner_name,
                    .type = .type,
                    .binding = .@"const",
                    .is_pub = sd.is_pub,
                    .value = .{
                        .type = switch (tag) {
                            .struct_declaration => .{ .@"struct" = .{
                                .name = try alloc.dupe(u8, sd.name),
                                .inner_name = try alloc.dupe(u8, inner_name),
                                .members = &.{},
                                .symbols = &.{},
                                .tag_type = tag_type,
                            } },
                            .union_declaration => .{ .@"union" = .{
                                .name = try alloc.dupe(u8, sd.name),
                                .inner_name = try alloc.dupe(u8, inner_name),
                                .members = &.{},
                                .symbols = &.{},
                                .tag_type = tag_type,
                            } },
                            .enum_declaration => .{ .@"enum" = .{
                                .name = try alloc.dupe(u8, sd.name),
                                .inner_name = try alloc.dupe(u8, inner_name),
                                .members = &.{},
                                .symbols = &.{},
                                .tag_type = tag_type,
                            } },
                            else => unreachable,
                        },
                    },
                    .free_name = true,
                    .free_type = true,
                    .free_inner_name = true,
                };
                c.module.registerPtr(alloc, symbol) catch |err| {
                    symbol.deinit(alloc);
                    alloc.destroy(symbol);
                    return err;
                };

                const ct_type = Type.CompoundType(t_tag);
                var members: std.ArrayList(ct_type.Member) = .empty;
                defer {
                    for (members.items) |member_m| member_m.deinit(alloc);
                    members.deinit(alloc);
                }

                var symbols: std.ArrayList(Symbol) = .empty;
                defer {
                    for (symbols.items) |sym| sym.deinit(alloc);
                    symbols.deinit(alloc);
                }

                try m.pushScope(alloc);
                defer m.popScope(alloc);

                for (sd.subtypes) |subtype| {
                    const st_name = switch (subtype) { inline else => |s| s.name };
                    const m_inner_name = try std.fmt.allocPrint(alloc, "{s}_{s}", .{ symbol.inner_name, st_name });
                    defer alloc.free(m_inner_name);

                    var forward_symbol = try alloc.create(Symbol);
                    forward_symbol.* = .{
                        .name = try alloc.dupe(u8, st_name),
                        .inner_name = try alloc.dupe(u8, m_inner_name),
                        .type = .type,
                        .binding = .@"const",
                        .is_pub = switch (subtype) { inline else => |s| s.is_pub },
                        .value = .{
                            .type = switch (subtype) {
                                .@"struct" => .{ .@"struct" = .{ .name = try alloc.dupe(u8, m_inner_name), .inner_name = try alloc.dupe(u8, m_inner_name), .members = &.{}, .symbols = &.{}, .tag_type = null } },
                                .@"union" => .{ .@"union" = .{ .name = try alloc.dupe(u8, m_inner_name), .inner_name = try alloc.dupe(u8, m_inner_name), .members = &.{}, .symbols = &.{}, .tag_type = null } },
                                .@"enum" => .{ .@"enum" = .{ .name = try alloc.dupe(u8, m_inner_name), .inner_name = try alloc.dupe(u8, m_inner_name), .members = &.{}, .symbols = &.{}, .tag_type = null } },
                            }
                        },
                        .free_name = true,
                        .free_type = true,
                        .free_inner_name = true,
                    };
                    m.registerPtr(alloc, forward_symbol) catch |err| {
                        forward_symbol.deinit(alloc);
                        alloc.destroy(forward_symbol);
                        return err;
                    };

                    var modified_st: ast.TopLevelStatement = switch (subtype) {
                        .@"struct" => |s| .{ .struct_declaration = try s.clone(alloc) },
                        .@"enum" => |e| .{ .enum_declaration = try e.clone(alloc) },
                        .@"union" => |u| .{ .union_declaration = try u.clone(alloc) },
                    };
                    defer modified_st.deinit(alloc);
                    switch (modified_st) {
                        inline .struct_declaration, .enum_declaration, .union_declaration => |*s| {
                            alloc.free(s.name);
                            s.name = try alloc.dupe(u8, m_inner_name);
                        },
                        else => unreachable,
                    }

                    try analyze(c, alloc, io, &[_]ast.TopLevelStatement{modified_st}, m);

                    const st_symbol = m.getSymbol(m_inner_name) orelse return error.UnknownSymbol;
                    
                    forward_symbol.value.?.type.deinit(alloc);
                    forward_symbol.value = .{ .type = try st_symbol.value.?.type.clone(alloc) };

                    try symbols.append(alloc, try forward_symbol.clone(alloc));
                }

                switch (tag) {
                    .struct_declaration => {
                        for (sd.members) |member| {
                            const member_t: Type = try .fromAst(alloc, io, &member.type, c, m);
                            try members.append(alloc, .{
                                .name = try alloc.dupe(u8, member.name),
                                .inner_name = try alloc.dupe(u8, member.name),
                                .type = member_t,
                            });
                        }
                    },
                    .union_declaration => for (sd.members) |member| {
                        const member_t: Type = if (member.type) |*t| try .fromAst(alloc, io, t, c, m) else .u8;
                        try members.append(alloc, .{
                            .name = try alloc.dupe(u8, member.name),
                            .inner_name = try alloc.dupe(u8, member.name),
                            .type = member_t,
                        });
                    },
                    .enum_declaration => {
                        for (sd.members, 0..) |member, i| {
                            const value: Value = if (member.value) |*v|
                                try .eval(alloc, io, v, c, m)
                            else
                                .{ .uint = if (i == 0) 0 else members.items[i - 1].value + 1 };

                            if (value != .uint)
                                return errors.enumMemberMustBeInteger(io, value.getType(), m.source_map[sd.pos]);

                            const m_inner_name = try std.fmt.allocPrint(alloc, "{s}_{s}", .{
                                symbol.inner_name,
                                member.name,
                            });
                            try members.append(alloc, .{
                                .name = try alloc.dupe(u8, member.name),
                                .inner_name = m_inner_name,
                                .value = value.uint,
                            });
                        }
                    },
                    else => unreachable,
                }

                const ct_ptr = switch (tag) {
                    .struct_declaration => &symbol.value.?.type.@"struct",
                    .union_declaration => &symbol.value.?.type.@"union",
                    .enum_declaration => &symbol.value.?.type.@"enum",
                    else => unreachable,
                };

                ct_ptr.members = try members.toOwnedSlice(alloc);

                for (sd.methods) |method| {
                    if (method.generic_parameters.len > 0) continue;

                    const m_inner_name = try std.fmt.allocPrint(alloc, "{s}_{s}", .{
                        symbol.inner_name,
                        method.name,
                    });

                    const method_t_ast = try method.getType(alloc);
                    defer method_t_ast.deinit(alloc);
                    const t: Type = try .fromAst(alloc, io, &method_t_ast, c, m);
                    const m_symbol = try alloc.create(Symbol);
                    m_symbol.* = .{
                        .name = try alloc.dupe(u8, method.name),
                        .inner_name = m_inner_name,
                        .type = t,
                        .binding = .@"const",
                        .is_pub = method.is_pub,
                        .free_name = true,
                        .free_type = true,
                        .free_inner_name = true,
                    };
                    c.module.registerPtr(alloc, m_symbol) catch |err| {
                        m_symbol.deinit(alloc);
                        alloc.destroy(m_symbol);
                        return err;
                    };

                    const cloned = try m_symbol.clone(alloc);
                    try symbols.append(alloc, cloned);

                    const return_t: Type = try .fromAst(alloc, io, &method.return_type, c, m);
                    defer return_t.deinit(alloc);
                }

                ct_ptr.symbols = try symbols.toOwnedSlice(alloc);
            },
            .import => |import_stmt| try statements.import(alloc, io, import_stmt, c, m),
        };
    }
};

pub fn emit(
    alloc: std.mem.Allocator,
    io: std.Io,
    rel_file_path: []const u8,
    module_registry: *std.StringHashMap(*Module),
) !void {
    var file_path = rel_file_path;
    var free_file_path = false;
    const input = std.Io.Dir.cwd().readFileAlloc(io, rel_file_path, alloc, .unlimited) catch |err| switch (err) {
        error.FileNotFound => b: {
            const stdlib_dir = try std.Io.Dir.openDirAbsolute(io, build_options.stdlib_path, .{});
            defer stdlib_dir.close(io);
            file_path = try std.fs.path.join(alloc, &.{ build_options.stdlib_path, rel_file_path });
            free_file_path = true;
            break :b try stdlib_dir.readFileAlloc(io, file_path, alloc, .unlimited);
        },
        else => return err,
    };
    defer alloc.free(input);
    defer if (free_file_path) alloc.free(file_path);

    const owned_file_path = try alloc.dupe(u8, file_path);
    const tokens, const source_map = try lexer.tokenize(alloc, io, input, owned_file_path);

    const root_node = try parser.parse(alloc, io, tokens, source_map);
    defer utils.deinitSlice(ast.TopLevelStatement, root_node, alloc);

    std.debug.assert(std.mem.eql(u8, file_path[file_path.len - 4 ..], ".zag"));

    const filename = std.fs.path.basename(file_path);
    const module_name = filename[0 .. filename.len - 4];
    var compiler: Compiler = .{
        .header = try .init(alloc),
        .source = try .init(alloc),
        .module = try .init(alloc, module_name, source_map),
        .primitives = .init(alloc),
        .module_registry = module_registry,
    };
    defer compiler.deinit(alloc);

    var @".zag-out" = try std.Io.Dir.cwd().createDirPathOpen(io, ".zag-out", .{});
    defer @".zag-out".close(io);

    const dir_name = std.fs.path.dirname(file_path).?;
    const relative_path = if (std.mem.eql(u8, dir_name, build_options.stdlib_path))
        try std.fs.path.join(alloc, &.{ "lib", file_path[dir_name.len .. file_path.len - 4] })
    else
        file_path[0 .. file_path.len - 4];
    defer if (std.mem.startsWith(u8, file_path, build_options.stdlib_path)) alloc.free(relative_path);

    const source_path = try std.fmt.allocPrint(alloc, "{s}.c", .{relative_path});
    defer alloc.free(source_path);

    const header_path = try std.fmt.allocPrint(alloc, "{s}.h", .{relative_path});
    defer alloc.free(header_path);

    try @".zag-out".createDirPath(io, std.fs.path.dirname(relative_path).?);

    try compiler.source.includes.print(alloc, "#include <{s}>\n", .{header_path});
    try compiler.source.includes.print(alloc, "#include <stddef.h>\n", .{});
    try compiler.source.includes.print(alloc, "#include <stdint.h>\n", .{});
    try compiler.source.includes.print(alloc, "#include <stdbool.h>\n", .{});

    try compiler.header.includes.print(alloc, "#include <stddef.h>\n", .{});
    try compiler.header.includes.print(alloc, "#include <stdint.h>\n", .{});
    try compiler.header.includes.print(alloc, "#include <stdbool.h>\n", .{});

    try compiler.analyze(alloc, io, root_node, compiler.module);

    // transpile step
    for (root_node) |statement|
        try statements.compileTopLevel(alloc, io, statement, &compiler, compiler.module);

    try compiler.source.includes.print(alloc, "int main() {{ {s}_main(); return 0; }}", .{module_name});

    var source_writer_buf: [1024]u8 = undefined;
    var source = try @".zag-out".createFile(io, source_path, .{});
    defer source.close(io);
    var source_writer = source.writer(io, &source_writer_buf);
    try compiler.source.write(&source_writer.interface);

    var header_writer_buf: [1024]u8 = undefined;
    var header = try @".zag-out".createFile(io, header_path, .{});
    defer header.close(io);
    var header_writer = header.writer(io, &header_writer_buf);
    try compiler.header.write(&header_writer.interface);
}

// caller owns return value
pub fn processImports(
    alloc: std.mem.Allocator,
    io: std.Io,
    rel_file_path: []const u8,
    module_registry: *std.StringHashMap(*Module),
) !*Module {
    var file_path = rel_file_path;
    var free_file_path = false;
    const input = std.Io.Dir.cwd().readFileAlloc(io, rel_file_path, alloc, .unlimited) catch |err| switch (err) {
        error.FileNotFound => b: {
            const stdlib_dir = try std.Io.Dir.openDirAbsolute(io, build_options.stdlib_path, .{});
            defer stdlib_dir.close(io);
            file_path = try std.fs.path.join(alloc, &.{ build_options.stdlib_path, rel_file_path });
            free_file_path = true;
            break :b try stdlib_dir.readFileAlloc(io, file_path, alloc, .unlimited);
        },
        else => return err,
    };
    defer alloc.free(input);
    defer if (free_file_path) alloc.free(file_path);

    if (module_registry.getPtr(file_path)) |mod| return mod.*;

    std.debug.assert(std.mem.eql(u8, file_path[file_path.len - 4 ..], ".zag"));

    const owned_file_path = try alloc.dupe(u8, file_path);
    const tokens, const source_map = try lexer.tokenize(alloc, io, input, owned_file_path);

    const root_node = try parser.parse(alloc, io, tokens, source_map);
    defer utils.deinitSlice(ast.TopLevelStatement, root_node, alloc);

    const filename = std.fs.path.basename(file_path);
    const module_name = filename[0 .. filename.len - 4];
    var compiler: Compiler = .{
        .header = try .init(alloc),
        .source = try .init(alloc),
        .module = try .init(alloc, module_name, source_map),
        .primitives = .init(alloc),
        .module_registry = module_registry,
    };
    const c = &compiler;
    defer compiler.deinitWithoutModule(alloc);

    try c.analyze(alloc, io, root_node, c.module);

    const gop = try module_registry.getOrPut(file_path);
    if (gop.found_existing) {
        c.module.deinit(alloc);
        return gop.value_ptr.*;
    }
    gop.value_ptr.* = c.module;

    return gop.value_ptr.*;
}
