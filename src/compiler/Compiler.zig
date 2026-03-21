const std = @import("std");
const ast = @import("ast");
const utils = @import("utils");

const parser = @import("parser");
const lexer = @import("lexer");

const statements = @import("statements.zig");

pub const Module = @import("Module.zig");

const Compiler = struct {
    source: struct {
        includes: std.ArrayList(u8) = .empty,
        type_impls: std.ArrayList(u8) = .empty,
        function_impls: std.ArrayList(u8) = .empty,

        fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
            self.includes.deinit(alloc);
            self.type_impls.deinit(alloc);
            self.function_impls.deinit(alloc);
        }

        fn write(self: *@This(), writer: *std.Io.Writer) !void {
            try writer.writeAll(self.includes.items);
            try writer.writeAll(self.type_impls.items);
            try writer.writeAll(self.function_impls.items);
        }
    } = .{},

    header: struct {
        includes: std.ArrayList(u8) = .empty,
        forward_decls: std.ArrayList(u8) = .empty,
        function_decls: std.ArrayList(u8) = .empty,

        fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
            self.includes.deinit(alloc);
            self.forward_decls.deinit(alloc);
            self.function_decls.deinit(alloc);
        }

        fn write(self: *@This(), writer: *std.Io.Writer) !void {
            try writer.writeAll(self.includes.items);
            try writer.writeAll(self.forward_decls.items);
            try writer.writeAll(self.function_decls.items);
        }
    } = .{},

    fn deinit(self: *Compiler, alloc: std.mem.Allocator) void {
        self.source.deinit(alloc);
        self.header.deinit(alloc);
    }
};

pub fn emit(alloc: std.mem.Allocator, file_path: []const u8) !void {
    const tokens, const tokens_source_map = try lexer.tokenize(alloc, file_path);
    defer alloc.free(tokens_source_map);

    const root_node = try parser.parse(alloc, tokens, tokens_source_map);
    defer utils.deinitSlice(ast.TopLevelStatement, root_node, alloc);

    var compiler: Compiler = .{};
    defer compiler.deinit(alloc);

    for (root_node) |statement| switch (statement) {
        else => {},
    };

    var @".zag-out" = try std.fs.cwd().makeOpenPath(".zag-out", .{});
    defer @".zag-out".close();

    const source_path = try std.fmt.allocPrint(alloc, "{s}.c", .{file_path[0 .. file_path.len - 4]});
    defer alloc.free(source_path);

    const header_path = try std.fmt.allocPrint(alloc, "{s}.h", .{file_path[0 .. file_path.len - 4]});
    defer alloc.free(header_path);

    try @".zag-out".makePath(std.fs.path.dirname(file_path).?);

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
