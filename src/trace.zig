const std = @import("std");
const build_options = @import("build_options");

pub const enabled: bool = build_options.trace;

/// Structured trace logger, gated by the -Dtrace build option.
/// Usage: trace.log("VM", "IP: {d} opcode={s}", .{ ip, @tagName(op) });
pub fn log(comptime tag: []const u8, comptime fmt: []const u8, args: anytype) void {
    if (!enabled) return;
    const full_fmt = "[" ++ tag ++ "] " ++ fmt ++ "\n";
    std.debug.print(full_fmt, args);
}
