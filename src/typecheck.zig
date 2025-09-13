const std = @import("std");
const ast = @import("ast.zig");
const types = @import("types.zig");

const Node = ast.Node;

pub const TypeChecker = struct {
    allocator: std.mem.Allocator,
    mode: types.Mode,
    diagnostics: std.ArrayList([]const u8),

    pub fn init(allocator: std.mem.Allocator, mode: types.Mode) TypeChecker {
        return .{
            .allocator = allocator,
            .mode = mode,
            .diagnostics = std.ArrayList([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *TypeChecker) void {
        for (self.diagnostics.items) |msg| self.allocator.free(msg);
        self.diagnostics.deinit();
    }

    pub fn check(self: *TypeChecker, root: *Node) anyerror!void {
        if (root.type != .program) return;

        // In strict mode, require annotations at public boundaries:
        // - function params and return type
        // - struct field types
        // - trait method param and return types
        // Local inference for variables allowed, but if no initializer, require declared type.
        for (root.data.program.body.items) |stmt| {
            try self.checkNode(stmt);
        }
    }

    fn checkNode(self: *TypeChecker, node: *Node) anyerror!void {
        switch (node.type) {
            .function_decl => try self.checkFunction(node),
            .struct_decl => try self.checkStruct(node),
            .trait_decl => try self.checkTrait(node),
            .impl_decl => try self.checkImpl(node),
            .variable_decl => try self.checkVar(node),
            .block_stmt => {
                for (node.data.block_stmt.statements.items) |s| {
                    try self.checkNode(s);
                }
            },
            .if_stmt => {
                try self.checkNode(node.data.if_stmt.then_branch);
                if (node.data.if_stmt.else_branch) |e| try self.checkNode(e);
            },
            .while_stmt => try self.checkNode(node.data.while_stmt.body),
            .for_stmt => {
                if (node.data.for_stmt.initializer) |i| try self.checkNode(i);
                if (node.data.for_stmt.increment) |inc| try self.checkNode(inc);
                try self.checkNode(node.data.for_stmt.body);
            },
            .expression_stmt => try self.checkExpr(node.data.expression_stmt.expr),
            .assignment => {
                try self.checkExpr(node.data.assignment.value);
            },
            .member_assign => {
                try self.checkExpr(node.data.member_assign.value);
            },
            .program => {
                for (node.data.program.body.items) |s| try self.checkNode(s);
            },
            else => {},
        }
    }

    fn checkFunction(self: *TypeChecker, node: *Node) anyerror!void {
        const fn_data = node.data.function_decl;
        if (self.mode == .strict) {
            // Require all param types annotated
            if (fn_data.param_types.items.len != fn_data.params.items.len) {
                try self.errFmt("Function '{s}': internal error param/types length mismatch", .{fn_data.name});
            } else {
                var i: usize = 0;
                while (i < fn_data.params.items.len) : (i += 1) {
                    if (fn_data.param_types.items[i] == null) {
                        try self.errFmt("Function '{s}': missing type annotation for parameter {d} ('{s}')", .{ fn_data.name, i, fn_data.params.items[i] });
                    }
                }
            }
            // Require return type annotation
            if (fn_data.return_type == null) {
                try self.errFmt("Function '{s}': missing return type annotation", .{fn_data.name});
            }
        }

        // Body
        try self.checkNode(fn_data.body);
    }

    fn checkStruct(self: *TypeChecker, node: *Node) anyerror!void {
        const sd = node.data.struct_decl;
        if (self.mode == .strict) {
            if (sd.field_types.items.len != sd.fields.items.len) {
                try self.errFmt("Struct '{s}': internal error fields/types length mismatch", .{sd.name});
            } else {
                var i: usize = 0;
                while (i < sd.fields.items.len) : (i += 1) {
                    if (sd.field_types.items[i] == null) {
                        try self.errFmt("Struct '{s}': missing type annotation for field '{s}'", .{ sd.name, sd.fields.items[i] });
                    }
                }
            }
        }
    }

    fn checkTrait(self: *TypeChecker, node: *Node) anyerror!void {
        const td = node.data.trait_decl;
        if (self.mode == .strict) {
            for (td.methods.items) |m| {
                if (m.param_types.items.len != m.params.items.len) {
                    try self.errFmt("Trait '{s}': method '{s}' internal error param/types length mismatch", .{ td.name, m.name });
                    continue;
                }
                var i: usize = 0;
                while (i < m.params.items.len) : (i += 1) {
                    if (m.param_types.items[i] == null) {
                        try self.errFmt("Trait '{s}': method '{s}' missing type for parameter {d} ('{s}')", .{ td.name, m.name, i, m.params.items[i] });
                    }
                }
                if (m.return_type == null) {
                    try self.errFmt("Trait '{s}': method '{s}' missing return type", .{ td.name, m.name });
                }
            }
        }
    }

    fn checkImpl(self: *TypeChecker, node: *Node) anyerror!void {
        _ = self;
        const id = node.data.impl_decl;
        // For Phase 2.5 foundation: verify methods have bodies; optional: arity match with trait declaration if available.
        // Minimal: ensure methods exist and have parameter lists; deep conformance will be added later.
        _ = id;
    }

    fn checkVar(self: *TypeChecker, node: *Node) anyerror!void {
        const vd = node.data.variable_decl;
        if (self.mode == .strict) {
            // If no declared type and no initializer, reject.
            if (vd.declared_type == null and vd.value == null) {
                try self.errFmt("Variable '{s}': missing type annotation or initializer in strict mode", .{vd.name});
            }
        }
        if (vd.value) |v| try self.checkExpr(v);
    }

    fn checkExpr(self: *TypeChecker, node: *Node) anyerror!void {
        // For foundational pass, do not compute precise types yet; recurse structure for obviously typed literals/expressions.
        switch (node.type) {
            .literal => {},
            .identifier => {},
            .binary_expr => {
                try self.checkExpr(node.data.binary_expr.left);
                try self.checkExpr(node.data.binary_expr.right);
            },
            .unary_expr => try self.checkExpr(node.data.unary_expr.right),
            .logical_expr => {
                try self.checkExpr(node.data.logical_expr.left);
                try self.checkExpr(node.data.logical_expr.right);
            },
            .call_expr => {
                try self.checkExpr(node.data.call_expr.callee);
                for (node.data.call_expr.args.items) |a| try self.checkExpr(a);
            },
            .member_expr => try self.checkExpr(node.data.member_expr.object),
            .struct_lit => {
                for (node.data.struct_lit.inits.items) |fi| try self.checkExpr(fi.expr);
            },
            .trait_cast => try self.checkExpr(node.data.trait_cast.object),
            .trait_invoke => {
                try self.checkExpr(node.data.trait_invoke.target);
                for (node.data.trait_invoke.args.items) |a| try self.checkExpr(a);
            },
            else => {},
        }
    }

    fn errFmt(self: *TypeChecker, comptime fmt: []const u8, args: anytype) anyerror!void {
        const msg = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.diagnostics.append(msg);
    }
};

pub const TypecheckResult = struct {
    ok: bool,
    diagnostics: std.ArrayList([]const u8),

    pub fn deinit(self: *TypecheckResult, allocator: std.mem.Allocator) void {
        for (self.diagnostics.items) |msg| allocator.free(msg);
        self.diagnostics.deinit();
    }
};

pub fn runTypecheck(allocator: std.mem.Allocator, root: *Node, mode: types.Mode) !TypecheckResult {
    var tc = TypeChecker.init(allocator, mode);
    defer tc.deinit();

    try tc.check(root);

    var diags = std.ArrayList([]const u8).init(allocator);
    for (tc.diagnostics.items) |msg| {
        try diags.append(try allocator.dupe(u8, msg));
    }

    return .{
        .ok = diags.items.len == 0,
        .diagnostics = diags,
    };
}
