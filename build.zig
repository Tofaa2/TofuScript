const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const trace_opt = b.option(bool, "trace", "Enable structured tracing (verbose VM/compiler dumps)") orelse false;

    // Build options for compile-time flags
    const options = b.addOptions();
    options.addOption(bool, "trace", trace_opt);

    // Main executable module
    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    // Expose options to the program as @import("build_options")
    exe_mod.addOptions("build_options", options);

    // Main executable
    const exe = b.addExecutable(.{
        .name = "TofuScript",
        .root_module = exe_mod,
    });

    b.installArtifact(exe);

    // Run the main application
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
