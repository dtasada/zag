const std = @import("std");
const clap = @import("clap");
const pretty = @import("pretty");
const builtin = @import("builtin");

const utils = @import("utils");
const Lexer = @import("Lexer");
const Parser = @import("Parser");
const Compiler = @import("Compiler");

const BuildError = error{
    FailedToReadSource,
    FailedToTokenizeSource,
    FailedToCreateParser,
    FailedToParseSource,
    FailedToCreateCompiler,
    CompilationError,
};

fn build(alloc: std.mem.Allocator) !void {
    const file_path = try std.fs.path.join(alloc, &.{ "src", "main.zag" });
    defer alloc.free(file_path);

    var buf: [65535]u8 = undefined;
    const file = std.fs.cwd().readFile(file_path, &buf) catch |err|
        return utils.printErr(
            error.FailedToReadSource,
            "Failed to open file '{s}': {}\n",
            .{ file_path, err },
            .red,
        );

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

    var compiler = Compiler.init(arena, parser, file_path) catch |err|
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

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const params = comptime clap.parseParamsComptime(
        \\-h, --help        Display this message and exit.
        \\<str>...
    );

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
        .allocator = alloc,
    }) catch |err| {
        try diag.reportToFile(.stderr(), err);
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0)
        return clap.helpToFile(.stdout(), clap.Help, &params, .{});
    for (res.positionals[0]) |pos| {
        if (std.mem.eql(u8, pos, "build")) {
            build(alloc) catch |err| {
                utils.print("Failed to build project: {}\n", .{err}, .red);
            };
        } else {
            utils.print("unhandled.\n", .{}, .white);
        }
    }
}
