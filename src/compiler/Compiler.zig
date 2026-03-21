const std = @import("std");
const ast = @import("ast");
const utils = @import("utils");
const build_options = @import("build_options");

const parser = @import("parser");
const lexer = @import("lexer");

const statements = @import("statements.zig");
const Type = @import("type.zig").Type;

pub const Module = @import("Module.zig");
pub const Value = @import("Value.zig").Value;

pub const Symbol = struct {
    name: []const u8,
    type: Type,
    binding: utils.Binding = .let,
    is_pub: bool = false,

    pub fn deinit(self: Symbol, alloc: std.mem.Allocator) void {
        _ = self;
        _ = alloc;
    }
};

pub const Compiler = struct {
    source: struct {
        includes: *std.ArrayList(u8),
        type_impls: *std.ArrayList(u8),
        function_impls: *std.ArrayList(u8),

        fn init(alloc: std.mem.Allocator) !@This() {
            const self: @This() = .{
                .includes = try alloc.create(std.ArrayList(u8)),
                .type_impls = try alloc.create(std.ArrayList(u8)),
                .function_impls = try alloc.create(std.ArrayList(u8)),
            };
            self.includes.* = .empty;
            self.type_impls.* = .empty;
            self.function_impls.* = .empty;
            return self;
        }

        fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
            self.includes.deinit(alloc);
            self.type_impls.deinit(alloc);
            self.function_impls.deinit(alloc);

            alloc.destroy(self.includes);
            alloc.destroy(self.type_impls);
            alloc.destroy(self.function_impls);
        }

        fn write(self: *@This(), writer: *std.Io.Writer) !void {
            try writer.writeAll(self.includes.items);
            try writer.writeAll(self.type_impls.items);
            try writer.writeAll(self.function_impls.items);
            try writer.flush();
        }
    },

    header: struct {
        includes: *std.ArrayList(u8),
        forward_decls: *std.ArrayList(u8),
        function_decls: *std.ArrayList(u8),

        fn init(alloc: std.mem.Allocator) !@This() {
            const self: @This() = .{
                .includes = try alloc.create(std.ArrayList(u8)),
                .forward_decls = try alloc.create(std.ArrayList(u8)),
                .function_decls = try alloc.create(std.ArrayList(u8)),
            };
            self.includes.* = .empty;
            self.forward_decls.* = .empty;
            self.function_decls.* = .empty;
            return self;
        }

        fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
            self.includes.deinit(alloc);
            self.forward_decls.deinit(alloc);
            self.function_decls.deinit(alloc);

            alloc.destroy(self.includes);
            alloc.destroy(self.forward_decls);
            alloc.destroy(self.function_decls);
        }

        fn write(self: *@This(), writer: *std.Io.Writer) !void {
            try writer.writeAll(self.includes.items);
            try writer.writeAll(self.forward_decls.items);
            try writer.writeAll(self.function_decls.items);
            try writer.flush();
        }
    },

    module: Module,
    source_map: []const utils.Position,

    fn deinit(self: *Compiler, alloc: std.mem.Allocator) void {
        self.source.deinit(alloc);
        self.header.deinit(alloc);
        self.module.deinit(alloc);
    }
};

pub fn emit(alloc: std.mem.Allocator, file_path: []const u8) !void {
    const tokens, const source_map = try lexer.tokenize(alloc, file_path);
    defer alloc.free(source_map);

    const root_node = try parser.parse(alloc, tokens, source_map);
    defer utils.deinitSlice(ast.TopLevelStatement, root_node, alloc);

    var compiler: Compiler = .{
        .header = try .init(alloc),
        .source = try .init(alloc),
        .module = try .init(alloc),
        .source_map = source_map,
    };
    defer compiler.deinit(alloc);

    for (root_node) |statement| try statements.compileTopLevel(alloc, &compiler, statement);

    var @".zag-out" = try std.fs.cwd().makeOpenPath(".zag-out", .{});
    defer @".zag-out".close();

    std.debug.assert(std.mem.eql(u8, file_path[file_path.len - 4 ..], ".zag"));

    const relative_path = if (std.mem.startsWith(u8, file_path, build_options.stdlib_path))
        try std.fmt.allocPrint(alloc, "lib/{s}", .{file_path[build_options.stdlib_path.len + 1 ..]})
    else
        file_path[0 .. file_path.len - 4];
    defer if (std.mem.startsWith(u8, file_path, build_options.stdlib_path)) alloc.free(relative_path);

    const source_path = try std.fmt.allocPrint(alloc, "{s}.c", .{relative_path});
    defer alloc.free(source_path);

    const header_path = try std.fmt.allocPrint(alloc, "{s}.h", .{relative_path});
    defer alloc.free(header_path);

    try @".zag-out".makePath(std.fs.path.dirname(relative_path).?);

    var source_writer_buf: [1024]u8 = undefined;
    var source = try @".zag-out".createFile(source_path, .{});
    defer source.close();
    var source_writer = source.writer(&source_writer_buf);
    try compiler.source.write(&source_writer.interface);

    var header_writer_buf: [1024]u8 = undefined;
    var header = try @".zag-out".createFile(header_path, .{});
    defer header.close();
    var header_writer = header.writer(&header_writer_buf);
    try compiler.header.write(&header_writer.interface);
}
