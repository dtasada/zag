const std = @import("std");

const utils = @import("utils");
const Compiler = @import("Compiler");

/// Takes zag code and lexes, parses and compiles it to C code.
pub fn transpile(
    alloc: std.mem.Allocator,
    file_path: []const u8,
    registry: *std.StringHashMap(*Compiler.Module),
) !void {
    const ast = try Compiler.getAST(alloc, file_path);

    var compiler = Compiler.init(alloc, ast.root, file_path, registry, .emit) catch |err|
        return utils.printErr(
            error.FailedToCreateCompiler,
            "Failed to create compiler: {}\n",
            .{err},
            .red,
        );
    defer compiler.deinit();

    // Call emit to build the module
    compiler.emit() catch |err| return utils.printErr(
        error.CompilationError,
        "Compilation error: {}\n",
        .{err},
        .red,
    );
}

/// Reads a directory and transpiles all necessary files
fn transpileModule(
    alloc: std.mem.Allocator,
    dir_path: []const u8,
    registry: *std.StringHashMap(*Compiler.Module),
) !void {
    var dir = try std.fs.cwd().openDir(dir_path, .{ .iterate = true });
    defer dir.close();

    var walker = try dir.walk(alloc);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.path, ".zag")) continue;

        const file_path = try std.fmt.allocPrint(alloc, "{s}/{s}", .{ dir_path, entry.path });
        defer alloc.free(file_path);

        try transpile(alloc, file_path, registry);
    }
}

pub fn build() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    var arena_back: std.heap.ArenaAllocator = .init(alloc);
    defer arena_back.deinit();
    const arena = arena_back.allocator();

    var registry = std.StringHashMap(*Compiler.Module).init(arena);
    try transpileModule(arena, "src", &registry);

    try compile(alloc);
}

/// Compiles C code into machine code.
pub fn compile(alloc: std.mem.Allocator) !void {
    const main_obj = try std.fs.path.join(alloc, &.{ ".zag-out", "bin", "main" });
    defer alloc.free(main_obj);

    const @"-Iinclude" = try std.fs.path.join(alloc, &.{ "-I./", ".zag-out", "zag" });
    defer alloc.free(@"-Iinclude");

    const src_path = try std.fs.path.join(alloc, &.{ ".zag-out", "src" });
    defer alloc.free(src_path);

    var @".zig-out/src" = try std.fs.cwd().openDir(src_path, .{ .iterate = true });
    defer @".zig-out/src".close();

    var files_it = @".zig-out/src".iterate();

    var files: std.ArrayList([]const u8) = .empty;
    defer files.deinit(alloc);
    defer for (files.items) |f| alloc.free(f);

    while (try files_it.next()) |file| {
        if (std.mem.endsWith(u8, file.name, ".c")) {
            try files.append(alloc, try std.fs.path.join(alloc, &.{ src_path, file.name }));
        }
    }

    const cmd_args = try std.mem.concat(alloc, []const u8, &.{
        &.{ "/usr/bin/cc", "-o", main_obj },
        files.items,
        &.{ @"-Iinclude", "-Wall", "-Wextra" },
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

    if (stdout.len != 0) utils.print("{s}\n", .{stdout}, .white);
    if (stderr.len != 0) utils.print("{s}\n", .{stderr}, .red);

    return checkResult(error.RuntimeError, result);
}

fn checkResult(comptime err: anyerror, result: std.process.Child.Term) !void {
    switch (result) {
        .Exited => |c| if (c != 0) return utils.printErr(err, "{}: exited with code {}.\n", .{ err, c }, .red),
        .Signal => |c| return utils.printErr(err, "{}: exited with signal {}.\n", .{ err, c }, .red),
        .Stopped => |c| return utils.printErr(err, "{}: stopped with code {}.\n", .{ err, c }, .red),
        .Unknown => |c| return utils.printErr(err, "{}: unknown problem with code {}.\n", .{ err, c }, .red),
    }
}
