const std = @import("std");
const ast = @import("ast.zig");
const types = @import("types.zig");

const Node = ast.Node;

pub const Resolver = struct {
    allocator: std.mem.Allocator,
    env: types.Env,
    diagnostics: std.ArrayList([]const u8),

    pub fn init(allocator: std.mem.Allocator) Resolver {
        return .{
            .allocator = allocator,
            .env = types.Env.init(allocator),
            .diagnostics = std.ArrayList([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *Resolver) void {
        for (self.diagnostics.items) |msg| {
            self.allocator.free(msg);
        }
        self.diagnostics.deinit();
        self.env.deinit();
    }

    pub fn resolve(self: *Resolver, root: *Node) !void {
        if (root.type != .program) return;

        // First pass: collect nominal types (structs/traits) into env
        for (root.data.program.body.items) |stmt| {
            try self.collectNominals(stmt);
        }

        // Further passes (variables, functions) could be added here later
    }

    fn collectNominals(self: *Resolver, node: *Node) !void {
        switch (node.type) {
            .struct_decl => {
                const name = node.data.struct_decl.name;
                if (self.env.isStruct(name)) {
                    try self.addDiagFmt("Duplicate struct '{s}'", .{name});
                } else {
                    try self.env.registerStruct(name);
                }
            },
            .trait_decl => {
                const name = node.data.trait_decl.name;
                if (self.env.isTrait(name)) {
                    try self.addDiagFmt("Duplicate trait '{s}'", .{name});
                } else {
                    try self.env.registerTrait(name);
                }
            },
            .function_decl,
            .variable_decl,
            .assignment,
            .binary_expr,
            .unary_expr,
            .logical_expr,
            .literal,
            .identifier,
            .call_expr,
            .import_stmt,
            .block_stmt,
            .expression_stmt,
            .if_stmt,
            .while_stmt,
            .for_stmt,
            .return_stmt,
            .break_stmt,
            .continue_stmt,
            .member_expr,
            .member_assign,
            .struct_lit,
            .impl_decl,
            .trait_cast,
            .trait_invoke,
            => {},
            .program => {
                for (node.data.program.body.items) |stmt| {
                    try self.collectNominals(stmt);
                }
            },
        }
    }

    pub fn getEnv(self: *Resolver) *types.Env {
        return &self.env;
    }

    fn addDiagFmt(self: *Resolver, comptime fmt: []const u8, args: anytype) !void {
        const msg = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.diagnostics.append(msg);
    }
};

pub const ResolveResult = struct {
    ok: bool,
    diagnostics: std.ArrayList([]const u8),

    pub fn deinit(self: *ResolveResult, allocator: std.mem.Allocator) void {
        for (self.diagnostics.items) |msg| allocator.free(msg);
        self.diagnostics.deinit();
    }
};

/// Convenience helper to run resolver and produce a result with owned diagnostics list.
pub fn runResolve(allocator: std.mem.Allocator, root: *Node) !ResolveResult {
    var resolver = Resolver.init(allocator);
    defer resolver.deinit();

    try resolver.resolve(root);

    var diags = std.ArrayList([]const u8).init(allocator);
    // Move diagnostics out
    for (resolver.diagnostics.items) |msg| {
        try diags.append(try allocator.dupe(u8, msg));
    }

    return ResolveResult{
        .ok = diags.items.len == 0,
        .diagnostics = diags,
    };
}
