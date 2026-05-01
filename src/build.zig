const std = @import("std");
const build_options = @import("build_options");

const utils = @import("utils");
const compiler = @import("compiler");

const Module = compiler.Module;

pub fn build(alloc: std.mem.Allocator, io: std.Io) !void {
    // Transpile stdlib first so user code can import from it
    var module_registry: std.StringHashMap(Module) = .init(alloc);
    defer {
        var it = module_registry.valueIterator();
        while (it.next()) |mod| mod.deinit(alloc);
        module_registry.deinit();
    }
    try transpileModuleWithHeaders(alloc, io, build_options.stdlib_path, &module_registry);
    try transpileModuleWithHeaders(alloc, io, "src", &module_registry);

    try compile(alloc, io);
}

fn transpileModuleWithHeaders(
    alloc: std.mem.Allocator,
    io: std.Io,
    dir_path: []const u8,
    module_registry: *std.StringHashMap(Module),
) !void {
    var dir = try std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true });
    defer dir.close(io);

    var walker = try dir.walk(alloc);
    defer walker.deinit();

    while (try walker.next(io)) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.path, ".zag")) continue;

        const file_path = try std.fs.path.join(alloc, &.{ dir_path, entry.path });
        defer alloc.free(file_path);

        try transpileWithHeaders(alloc, io, file_path, module_registry);
    }
}

fn transpileWithHeaders(
    alloc: std.mem.Allocator,
    io: std.Io,
    file_path: []const u8,
    module_registry: *std.StringHashMap(Module),
) !void {
    compiler.emit(alloc, io, file_path, module_registry) catch |err|
        return utils.printErr(io, error.CompilationError, "Compilation error: {}\n", .{err});
}

/// Compiles C code into machine code.
pub fn compile(alloc: std.mem.Allocator, io: std.Io) !void {
    var @".zag-out" = try std.Io.Dir.cwd().openDir(io, ".zag-out", .{});
    defer @".zag-out".close(io);

    var @".zag-out/bin" = try @".zag-out".createDirPathOpen(io, "bin", .{});
    defer @".zag-out/bin".close(io);

    const main_obj = try std.fs.path.join(alloc, &.{ ".zag-out", "bin", "main" });
    defer alloc.free(main_obj);

    const src_path = try std.fs.path.join(alloc, &.{ ".zag-out", "src" });
    defer alloc.free(src_path);

    // const lib_path = try std.fs.path.join(alloc, &.{ ".zag-out", "lib" });
    // defer alloc.free(lib_path);

    var @".zag-out/src" = try std.Io.Dir.cwd().openDir(io, src_path, .{ .iterate = true });
    defer @".zag-out/src".close(io);

    // var @".zag-out/lib" = try std.fs.cwd().openDir(lib_path, .{ .iterate = true });
    // defer @".zag-out/lib".close();

    var files_src_it = @".zag-out/src".iterate();
    // var files_lib_it = @".zag-out/lib".iterate();

    var files: std.ArrayList([]const u8) = .empty;
    defer files.deinit(alloc);
    defer for (files.items) |f| alloc.free(f);

    while (try files_src_it.next(io)) |file| if (std.mem.endsWith(u8, file.name, ".c")) {
        try files.append(alloc, try std.fs.path.join(alloc, &.{ src_path, file.name }));
    };

    // while (try files_lib_it.next()) |file| if (std.mem.endsWith(u8, file.name, ".c")) {
    //     try files.append(alloc, try std.fs.path.join(alloc, &.{ lib_path, file.name }));
    // };

    var headers = try alloc.alloc([]const u8, files.items.len);
    defer {
        for (headers) |header| alloc.free(header);
        alloc.free(headers);
    }
    for (files.items, 0..) |file, i| headers[i] = try std.mem.replaceOwned(u8, alloc, file, ".c", ".h");
    const clang_format_args = try std.mem.concat(alloc, []const u8, &.{
        &.{ "clang-format", "-i" },
        files.items,
        headers,
    });
    defer alloc.free(clang_format_args);
    _ = try std.process.run(alloc, io, .{ .argv = clang_format_args });

    const cmd_args = try std.mem.concat(alloc, []const u8, &.{
        &.{ "/usr/bin/cc", "-g", "-o", main_obj },
        files.items,
        &.{
            "-I.zag-out/",
            // "-Wall",
            // "-Wextra",
            "-Wno-parentheses-equality",
            "-Wno-sign-compare",
            "-Wno-logical-op-parentheses",
            "-Wno-incompatible-pointer-types-discards-qualifiers",
            "-Wno-incompatible-pointer-types",
            // "-lraylib",
        },
    });
    defer alloc.free(cmd_args);

    for (cmd_args) |arg| utils.print(io, "{s} ", .{arg}, .white);
    utils.print(io, "\n", .{}, .white);

    const cc = try std.process.run(alloc, io, .{ .argv = cmd_args });
    defer alloc.free(cc.stdout);
    defer alloc.free(cc.stderr);
    if (cc.stdout.len != 0) utils.print(io, "C compiler output:\n{s}\n", .{cc.stdout}, .white);
    if (cc.stderr.len != 0) utils.print(io, "C compiler error output:\n{s}\n", .{cc.stderr}, .red);

    return checkResult(io, error.CompilationError, cc.term);
}

pub fn run(alloc: std.mem.Allocator, io: std.Io) !void {
    build(alloc, io) catch |err| {
        utils.print(io, "Build failed: {}\n", .{err}, .red);
        return err;
    };

    const exec_path = try std.fs.path.join(alloc, &.{ ".zag-out", "bin", "main" });
    defer alloc.free(exec_path);

    utils.print(io, "{s}\n", .{exec_path}, .white);

    const main = try std.process.run(alloc, io, .{ .argv = &.{exec_path} });
    defer alloc.free(main.stdout);
    defer alloc.free(main.stderr);
    if (main.stdout.len != 0) utils.print(io, "{s}{s}", .{ main.stdout, if (main.stdout[main.stdout.len - 1] == '\n') "" else "\n" }, .white);
    if (main.stderr.len != 0) utils.print(io, "{s}{s}", .{ main.stderr, if (main.stdout[main.stdout.len - 1] == '\n') "" else "\n" }, .red);

    return checkResult(io, error.RuntimeError, main.term);
}

fn checkResult(io: std.Io, comptime err: anyerror, result: std.process.Child.Term) !void {
    switch (result) {
        .exited => |c| if (c != 0) return utils.printErr(io, err, "{}: exited with code {}.\n", .{ err, c }),
        .signal => |c| return utils.printErr(io, err, "{}: exited with signal {}.\n", .{ err, c }),
        .stopped => |c| return utils.printErr(io, err, "{}: stopped with code {}.\n", .{ err, c }),
        .unknown => |c| return utils.printErr(io, err, "{}: unknown problem with code {}.\n", .{ err, c }),
    }
}
