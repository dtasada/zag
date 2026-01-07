const std = @import("std");

const utils = @import("utils");
const Lexer = @import("Lexer");
const Parser = @import("Parser");
const Compiler = @import("Compiler");

/// Takes zag code and lexes, parses and compiles it to C code.
pub fn transpile(alloc: std.mem.Allocator, file_path: []const u8) !void {
    const file = std.fs.cwd().readFileAlloc(alloc, file_path, 1024 * 1024) catch |err|
        return utils.printErr(
            error.FailedToReadSource,
            "Failed to open file '{s}': {}\n",
            .{ file_path, err },
            .red,
        );
    defer alloc.free(file);

    // use ArenaAllocator to avoid too many `.deinit()` methods.
    var arena_back = std.heap.ArenaAllocator.init(alloc);
    const arena = arena_back.allocator();
    defer arena_back.deinit();

    var lexer = Lexer.init(file, arena) catch |err|
        return utils.printErr(
            error.FailedToTokenizeSource,
            "Failed to tokenize source code: {}\n",
            .{err},
            .red,
        );
    defer lexer.deinit(arena);

    // for (lexer.tokens.items) |t| std.debug.print("t: {f}\n", .{t});

    var parser = Parser.init(lexer, arena) catch |err|
        return utils.printErr(
            error.FailedToCreateParser,
            "Failed to create parser: {}\n",
            .{err},
            .red,
        );
    defer parser.deinit();

    // try pretty.print(alloc, .{parser.output}, .{ .max_depth = 100 });

    var compiler = Compiler.init(arena, parser.output.items, file_path) catch |err|
        return utils.printErr(
            error.FailedToCreateCompiler,
            "Failed to create compiler: {}\n",
            .{err},
            .red,
        );
    defer compiler.deinit();

    compiler.emit() catch |err| // Call emit to build the module
        return utils.printErr(
            error.CompilationError,
            "Compilation error: {}\n",
            .{err},
            .red,
        );
}

/// Compiles C code into machine code.
pub fn compile(alloc: std.mem.Allocator) !void {
    const main_obj = try std.fs.path.join(alloc, &.{ ".zag-out", "bin", "main" });
    defer alloc.free(main_obj);

    const @"-Iinclude" = try std.fs.path.join(alloc, &.{ "-I./", ".zag-out", "zag" });
    defer alloc.free(@"-Iinclude");

    const src_path = try std.fs.path.join(alloc, &.{ ".zag-out", "src" });
    defer alloc.free(src_path);

    const @".zig-out/src" = try std.fs.cwd().openDir(src_path, .{ .iterate = true });
    var files_it = @".zig-out/src".iterate();

    var files: std.ArrayList([]const u8) = .empty;
    defer files.deinit(alloc);
    defer for (files.items) |f| alloc.free(f);

    while (try files_it.next()) |file|
        try files.append(alloc, try std.fs.path.join(alloc, &.{ src_path, file.name }));

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

    _ = cc.wait() catch return;

    if (stdout.len != 0) utils.print("C compiler output:\n{s}\n", .{stdout}, .white);
    if (stderr.len != 0) utils.print("C compiler error output:\n{s}\n", .{stderr}, .red);
}

pub fn build() anyerror!void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const src_path = "src";
    const src = try std.fs.cwd().openDir(src_path, .{ .iterate = true });
    var files_it = src.iterate();

    while (try files_it.next()) |file| {
        if (!std.mem.endsWith(u8, file.name, ".zag")) continue;

        const file_path = try std.fs.path.join(alloc, &.{ src_path, file.name });
        defer alloc.free(file_path);

        try transpile(alloc, file_path);
    }

    compile(alloc) catch |err| return utils.printErr(
        error.FailedToCompileTarget,
        "Build error: {}\n",
        .{err},
        .red,
    );
}

pub fn run() anyerror!void {
    try build();

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

    _ = main.wait() catch return;

    if (stdout.len != 0) utils.print("{s}\n", .{stdout}, .white);
    if (stderr.len != 0) utils.print("{s}\n", .{stderr}, .red);
}
