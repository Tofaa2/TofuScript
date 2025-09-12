const std = @import("std");
const tokens = @import("tokens.zig");
const Token = tokens.Token;

pub const NodeType = enum {
    program,
    function_decl,
    variable_decl,
    assignment,
    binary_expr,
    literal,
    identifier,
    call_expr,
    import_stmt,
    block_stmt,
};

pub const LiteralValue = union(enum) {
    number: f64,
    string: []const u8,
    boolean: bool,
    nil,
};

pub const Node = struct {
    type: NodeType,
    // Each node type will use different fields
    // We'll use a union-like pattern with optional fields
    data: Data,

    pub const Data = union {
        program: struct {
            body: std.ArrayList(*Node),
        },
        function_decl: struct {
            name: []const u8,
            params: std.ArrayList([]const u8),
            body: *Node, // block statement
        },
        variable_decl: struct {
            name: []const u8,
            value: ?*Node, // optional initializer
        },
        assignment: struct {
            name: []const u8,
            value: *Node,
        },
        binary_expr: struct {
            left: *Node,
            operator: Token,
            right: *Node,
        },
        literal: struct {
            value: LiteralValue,
        },
        identifier: struct {
            name: []const u8,
        },
        call_expr: struct {
            callee: *Node,
            args: std.ArrayList(*Node),
        },
        import_stmt: struct {
            module: []const u8,
        },
        block_stmt: struct {
            statements: std.ArrayList(*Node),
        },
    };

    pub fn init(allocator: std.mem.Allocator, node_type: NodeType) !*Node {
        const node = try allocator.create(Node);
        node.type = node_type;

        // Initialize data based on node type
        switch (node_type) {
            .program => node.data = .{ .program = .{ .body = std.ArrayList(*Node).init(allocator) } },
            .function_decl => node.data = .{ .function_decl = .{
                .name = "",
                .params = std.ArrayList([]const u8).init(allocator),
                .body = undefined,
            } },
            .variable_decl => node.data = .{ .variable_decl = .{
                .name = "",
                .value = null,
            } },
            .assignment => node.data = .{ .assignment = .{
                .name = "",
                .value = undefined,
            } },
            .binary_expr => node.data = .{ .binary_expr = .{
                .left = undefined,
                .operator = undefined,
                .right = undefined,
            } },
            .literal => node.data = .{ .literal = .{ .value = .{ .nil = {} } } },
            .identifier => node.data = .{ .identifier = .{ .name = "" } },
            .call_expr => node.data = .{ .call_expr = .{
                .callee = undefined,
                .args = std.ArrayList(*Node).init(allocator),
            } },
            .import_stmt => node.data = .{ .import_stmt = .{ .module = "" } },
            .block_stmt => node.data = .{ .block_stmt = .{
                .statements = std.ArrayList(*Node).init(allocator),
            } },
        }

        return node;
    }

    pub fn deinit(self: *Node, allocator: std.mem.Allocator) void {
        switch (self.type) {
            .program => self.data.program.body.deinit(),
            .function_decl => {
                self.data.function_decl.params.deinit();
                self.data.function_decl.body.deinit(allocator);
            },
            .variable_decl => if (self.data.variable_decl.value) |value| {
                value.deinit(allocator);
            },
            .assignment => self.data.assignment.value.deinit(allocator),
            .binary_expr => {
                self.data.binary_expr.left.deinit(allocator);
                self.data.binary_expr.right.deinit(allocator);
            },
            .literal => if (self.data.literal.value == .string) {
                allocator.free(self.data.literal.value.string);
            },
            .identifier => allocator.free(self.data.identifier.name),
            .call_expr => {
                for (self.data.call_expr.args.items) |arg| {
                    arg.deinit(allocator);
                }
                self.data.call_expr.args.deinit();
                self.data.call_expr.callee.deinit(allocator);
            },
            .import_stmt => allocator.free(self.data.import_stmt.module),
            .block_stmt => {
                for (self.data.block_stmt.statements.items) |stmt| {
                    stmt.deinit(allocator);
                }
                self.data.block_stmt.statements.deinit();
            },
        }
        allocator.destroy(self);
    }
};
