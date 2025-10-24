const std = @import("std");
const clap = @import("clap");
const pretty = @import("pretty");

const utils = @import("utils.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("parser/Parser.zig");

fn build(alloc: std.mem.Allocator) !void {
    utils.print("building...\n", .{}, .white);

    var buf: [65535]u8 = undefined;
    const file = std.fs.cwd().readFile("src/main.dl", &buf) catch |err| {
        utils.print("Failed to read 'src/main.dl': {}\n", .{err}, .red);
        return error.FailedToReadSource;
    };

    // use ArenaAllocator to avoid too many `.deinit()` methods.
    var arena_back = std.heap.ArenaAllocator.init(alloc);
    const arena = arena_back.allocator();
    defer arena_back.deinit();

    var lexer = Lexer.init(file);
    defer lexer.deinit(arena);
    lexer.tokenize(arena) catch |err| {
        utils.print("Couldn't get tokens: {}\n", .{err}, .red);
        return;
    };

    // for (lexer.tokens.items) |t| std.debug.print("t: {f}\n", .{t});

    var parser = Parser.init(&lexer, arena) catch |err| {
        utils.print("Failed to create parser: {}\n", .{err}, .red);
        return error.ParserFailed;
    };
    defer parser.deinit();
    const ast = parser.getAst(arena) catch |err| {
        utils.print("Failed to parse program: {}\n", .{err}, .red);
        return error.ParserFailed;
    };

    try pretty.print(alloc, .{ast}, .{ .max_depth = 100 });
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
