const std = @import("std");

const Modules = struct {
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,

    const Module = struct {
        name: []const u8,
        mod: *std.Build.Module,

        fn addImport(lhs: *const Module, rhs: *const Module) void {
            lhs.mod.addImport(rhs.name, rhs.mod);
        }
    };

    fn init(b: *std.Build, target: std.Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) Modules {
        return .{
            .b = b,
            .target = target,
            .optimize = optimize,
        };
    }

    fn create(self: Modules, name: []const u8, source_file: []const u8) Module {
        return .{
            .name = name,
            .mod = self.b.createModule(.{
                .target = self.target,
                .optimize = self.optimize,
                .root_source_file = self.b.path(source_file),
            }),
        };
    }
};

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const modules = Modules.init(b, target, optimize);
    const zag_mod = modules.create("zag", "src/main.zig");
    const exe = b.addExecutable(.{
        .name = "zag",
        .root_module = zag_mod.mod,
    });

    const utils_mod = modules.create("utils", "src/utils.zig");
    const lexer_mod = modules.create("Lexer", "src/Lexer.zig");
    const parser_mod = modules.create("Parser", "src/parser/Parser.zig");
    const compiler_mod = modules.create("Compiler", "src/compiler/Compiler.zig");

    zag_mod.addImport(&lexer_mod);
    zag_mod.addImport(&parser_mod);
    zag_mod.addImport(&compiler_mod);
    zag_mod.addImport(&utils_mod);

    parser_mod.addImport(&utils_mod);
    lexer_mod.addImport(&utils_mod);
    compiler_mod.addImport(&utils_mod);
    parser_mod.addImport(&lexer_mod);

    compiler_mod.addImport(&parser_mod);

    const cli = b.dependency("cli", .{});
    const cli_mod = cli.module("cli");
    exe.root_module.addImport("cli", cli_mod);

    const pretty = b.dependency("pretty", .{});
    const pretty_mod = pretty.module("pretty");
    exe.root_module.addImport("pretty", pretty_mod);
    compiler_mod.mod.addImport("pretty", pretty_mod);

    b.installArtifact(exe);

    const run_step = b.step("run", "Run the app");

    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const check = b.step("check", "Check if zag compiles");
    check.dependOn(&exe.step);
}
