const std = @import("std");
const ast = @import("ast");
const utils = @import("utils");
const build_options = @import("build_options");

const parser = @import("parser");
const lexer = @import("lexer");

const errors = @import("errors.zig");
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
    free_inner_name: bool,

    pub fn deinit(self: Symbol, alloc: std.mem.Allocator) void {
        if (self.free_type) self.type.deinit(alloc);
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

    pub fn clone(self: Symbol, alloc: std.mem.Allocator) !Symbol {
        const inner_name = if (self.free_inner_name) try alloc.dupe(u8, self.inner_name) else self.inner_name;
        errdefer if (self.free_inner_name) alloc.free(inner_name);

        const t = if (self.free_type) try self.type.clone(alloc) else self.type;
        errdefer if (self.free_type) t.deinit(alloc);

        const value = if (self.value) |v| try v.clone(alloc) else null;
        errdefer if (value) |v| v.deinit(alloc);

        return .{
            .name = self.name,
            .inner_name = inner_name,
            .type = t,
            .binding = self.binding,
            .is_pub = self.is_pub,
            .value = value,
            .free_type = self.free_type,
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
    module: Module,
    source_map: []const utils.Position,

    fn deinit(self: *Compiler, alloc: std.mem.Allocator) void {
        self.source.deinit(alloc);
        self.header.deinit(alloc);
        self.module.deinit(alloc);
        self.primitives.deinit();
    }

    /// Caller owns memory
    pub fn compileType(
        self: *Compiler,
        alloc: std.mem.Allocator,
        io: std.Io,
        t: *const Type,
        pos: usize,
    ) ![]const u8 {
        return switch (t.*) {
            .optional => |optional| {
                const type_name = try std.fmt.allocPrint(alloc, "__zag_Optional_{x}", .{optional.hash()});
                if (!self.primitives.contains(type_name)) {
                    const t_comp = try self.compileType(alloc, io, optional, pos);
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
                const inner = try self.compileType(alloc, io, ref.inner, pos);
                defer alloc.free(inner);
                return try std.fmt.allocPrint(alloc, "{s}{s}*", .{
                    if (!ref.is_mut) "const " else "",
                    inner,
                });
            },
            .variadic => unreachable,
            .@"struct", .@"union", .@"enum" => {
                const symbol = self.module.findSymbolByType(t.*).?;
                return try alloc.dupe(u8, symbol.inner_name);
            },
            .array => |array| {
                const type_name = try std.fmt.allocPrint(alloc, "__zag_Array_{x}", .{t.hash()});
                if (!self.primitives.contains(type_name)) {
                    const t_comp = try self.compileType(alloc, io, array.inner, pos);
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
            inline else => |_, tag| if (self.module.getSymbol(@tagName(tag))) |symbol|
                alloc.dupe(u8, symbol.inner_name)
            else
                errors.unknownSymbol(io, @tagName(tag), self.source_map[pos]),
        };
    }
};

pub fn emit(alloc: std.mem.Allocator, io: std.Io, file_path: []const u8) !void {
    const tokens, const source_map = try lexer.tokenize(alloc, io, file_path);
    defer alloc.free(source_map);

    const root_node = try parser.parse(alloc, io, tokens, source_map);
    defer utils.deinitSlice(ast.TopLevelStatement, root_node, alloc);

    std.debug.assert(std.mem.eql(u8, file_path[file_path.len - 4 ..], ".zag"));

    const module_name = std.fs.path.basename(file_path)[0 .. std.fs.path.basename(file_path).len - 4];
    var compiler: Compiler = .{
        .header = try .init(alloc),
        .source = try .init(alloc),
        .module = try .init(alloc, module_name),
        .source_map = source_map,
        .primitives = .init(alloc),
    };
    defer compiler.deinit(alloc);

    var @".zag-out" = try std.Io.Dir.cwd().createDirPathOpen(io, ".zag-out", .{});
    defer @".zag-out".close(io);

    const relative_path = if (std.mem.startsWith(u8, file_path, build_options.stdlib_path))
        try std.fmt.allocPrint(alloc, "lib/{s}", .{file_path[build_options.stdlib_path.len + 1 ..]})
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

    for (root_node) |statement| try statements.compileTopLevel(alloc, io, statement, &compiler);

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
