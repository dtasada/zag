const std = @import("std");
const build_options = @import("build_options");

const utils = @import("utils");
const compiler = @import("compiler");

pub fn build() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    // Transpile stdlib first so user code can import from it
    // try transpileModuleWithHeaders(alloc, build_options.stdlib_path);
    try transpileModuleWithHeaders(alloc, "src");

    // try compile(alloc);
}

fn transpileModuleWithHeaders(alloc: std.mem.Allocator, dir_path: []const u8) !void {
    var dir = try std.fs.cwd().openDir(dir_path, .{ .iterate = true });
    defer dir.close();

    var walker = try dir.walk(alloc);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.path, ".zag")) continue;

        const file_path = try std.fmt.allocPrint(alloc, "{s}/{s}", .{ dir_path, entry.path });
        defer alloc.free(file_path);

        try transpileWithHeaders(alloc, file_path);
    }
}

fn transpileWithHeaders(alloc: std.mem.Allocator, file_path: []const u8) !void {
    compiler.emit(alloc, file_path) catch |err|
        return utils.printErr(error.CompilationError, "Compilation error: {}\n", .{err});
}

/// Compiles C code into machine code.
pub fn compile(alloc: std.mem.Allocator) !void {
    var @".zag-out" = try std.fs.cwd().openDir(".zag-out", .{});
    defer @".zag-out".close();

    var @".zag-out/bin" = try @".zag-out".makeOpenPath("bin", .{});
    defer @".zag-out/bin".close();

    const main_obj = try std.fs.path.join(alloc, &.{ ".zag-out", "bin", "main" });
    defer alloc.free(main_obj);

    const src_path = try std.fs.path.join(alloc, &.{ ".zag-out", "src" });
    defer alloc.free(src_path);

    const lib_path = try std.fs.path.join(alloc, &.{ ".zag-out", "lib" });
    defer alloc.free(lib_path);

    var @".zag-out/src" = try std.fs.cwd().openDir(src_path, .{ .iterate = true });
    defer @".zag-out/src".close();

    var @".zag-out/lib" = try std.fs.cwd().openDir(lib_path, .{ .iterate = true });
    defer @".zag-out/lib".close();

    var files_src_it = @".zag-out/src".iterate();
    var files_lib_it = @".zag-out/lib".iterate();

    var files: std.ArrayList([]const u8) = .empty;
    defer files.deinit(alloc);
    defer for (files.items) |f| alloc.free(f);

    while (try files_src_it.next()) |file| if (std.mem.endsWith(u8, file.name, ".c")) {
        try files.append(alloc, try std.fs.path.join(alloc, &.{ src_path, file.name }));
    };

    while (try files_lib_it.next()) |file| if (std.mem.endsWith(u8, file.name, ".c")) {
        try files.append(alloc, try std.fs.path.join(alloc, &.{ lib_path, file.name }));
    };

    const clang_format_args = try std.mem.concat(alloc, []const u8, &.{
        &.{ "clang-format", "-i" },
        files.items,
    });
    defer alloc.free(clang_format_args);
    _ = try std.process.Child.run(.{
        .allocator = alloc,
        .argv = clang_format_args,
    });

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
            "-lraylib",
        },
    });
    defer alloc.free(cmd_args);

    for (cmd_args) |arg| utils.print("{s} ", .{arg}, .white);
    utils.print("\n", .{}, .white);

    var cc: std.process.Child = .init(cmd_args, alloc);

    cc.stdin_behavior = .Ignore;
    cc.stdout_behavior = .Pipe;
    cc.stderr_behavior = .Pipe;
    cc.spawn() catch |err| {
        utils.print("Couldn't spawn compiler command: {}\n", .{err}, .red);
        return;
    };
    const stdout = cc.stdout.?.readToEndAlloc(alloc, 1 << 20) catch return;
    defer alloc.free(stdout);

    const stderr = cc.stderr.?.readToEndAlloc(alloc, 1 << 20) catch return;
    defer alloc.free(stderr);

    const result = cc.wait() catch |err| {
        utils.print("Build failed: {}\n", .{err}, .red);
        return err;
    };

    if (stdout.len != 0) utils.print("C compiler output:\n{s}\n", .{stdout}, .white);
    if (stderr.len != 0) utils.print("C compiler error output:\n{s}\n", .{stderr}, .red);

    return checkResult(error.CompilationError, result);
}

pub fn run() anyerror!void {
    build() catch |err| {
        utils.print("Build failed: {}\n", .{err}, .red);
        return err;
    };

    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const exec_path = try std.fs.path.join(alloc, &.{ ".zag-out", "bin", "main" });
    defer alloc.free(exec_path);

    utils.print("{s}\n", .{exec_path}, .white);

    var main: std.process.Child = .init(&.{exec_path}, alloc);

    main.stdin_behavior = .Inherit;
    main.stdout_behavior = .Pipe;
    main.stderr_behavior = .Pipe;
    main.spawn() catch |err| {
        utils.print("Couldn't spawn : {}\n", .{err}, .red);
        return;
    };
    const stdout = main.stdout.?.readToEndAlloc(alloc, 1 << 20) catch return;
    defer alloc.free(stdout);

    const stderr = main.stderr.?.readToEndAlloc(alloc, 1 << 20) catch return;
    defer alloc.free(stderr);

    const result = main.wait() catch |err| {
        utils.print("Running failed: {}\n", .{err}, .red);
        return err;
    };

    if (stdout.len != 0) utils.print("{s}{s}", .{ stdout, if (stdout[stdout.len - 1] == '\n') "" else "\n" }, .white);
    if (stderr.len != 0) utils.print("{s}{s}", .{ stderr, if (stdout[stdout.len - 1] == '\n') "" else "\n" }, .red);

    return checkResult(error.RuntimeError, result);
}

fn checkResult(comptime err: anyerror, result: std.process.Child.Term) !void {
    switch (result) {
        .Exited => |c| if (c != 0) return utils.printErr(err, "{}: exited with code {}.\n", .{ err, c }),
        .Signal => |c| return utils.printErr(err, "{}: exited with signal {}.\n", .{ err, c }),
        .Stopped => |c| return utils.printErr(err, "{}: stopped with code {}.\n", .{ err, c }),
        .Unknown => |c| return utils.printErr(err, "{}: unknown problem with code {}.\n", .{ err, c }),
    }
}
