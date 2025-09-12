const std = @import("std");
const tokens = @import("tokens.zig");
const Token = tokens.Token;

pub const NodeType = enum {
    program,
    function_decl,
    variable_decl,
    assignment,
    binary_expr,
    unary_expr,
    logical_expr,
    literal,
    identifier,
    call_expr,
    import_stmt,
    block_stmt,
    expression_stmt,
    if_stmt,
    while_stmt,
    for_stmt,
    return_stmt,
    break_stmt,
    continue_stmt,
    // Structs
    member_expr,
    member_assign,
    struct_lit,
    struct_decl,
    trait_decl,
    impl_decl,
    // Traits runtime ops
    trait_cast,
    trait_invoke,
};

pub const LiteralValue = union(enum) {
    number: f64,
    string: []const u8,
    boolean: bool,
    nil,
};

pub const FieldInit = struct {
    name: []const u8,
    expr: *Node,
};

pub const MethodSig = struct {
    name: []const u8,
    params: std.ArrayList([]const u8),
};

pub const MethodImpl = struct {
    name: []const u8,
    params: std.ArrayList([]const u8),
    body: *Node,
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
        unary_expr: struct {
            operator: Token,
            right: *Node,
        },
        logical_expr: struct {
            left: *Node,
            operator: Token, // 'and' or 'or'
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
        expression_stmt: struct {
            expr: *Node,
        },
        if_stmt: struct {
            condition: *Node,
            then_branch: *Node,
            else_branch: ?*Node,
        },
        while_stmt: struct {
            condition: *Node,
            body: *Node,
        },
        for_stmt: struct {
            initializer: ?*Node,
            condition: ?*Node,
            increment: ?*Node,
            body: *Node,
        },
        return_stmt: struct {
            value: ?*Node,
        },
        break_stmt: struct {},
        continue_stmt: struct {},
        // member access and struct literals
        member_expr: struct {
            object: *Node,
            field: []const u8,
        },
        member_assign: struct {
            object: *Node,
            field: []const u8,
            value: *Node,
        },
        struct_lit: struct {
            type_name: []const u8,
            inits: std.ArrayList(FieldInit),
        },
        // declarations
        struct_decl: struct {
            name: []const u8,
            fields: std.ArrayList([]const u8),
        },
        trait_decl: struct {
            name: []const u8,
            methods: std.ArrayList(MethodSig),
        },
        impl_decl: struct {
            trait_name: []const u8,
            type_name: []const u8,
            methods: std.ArrayList(MethodImpl),
        },
        trait_cast: struct {
            object: *Node,
            trait_name: []const u8,
        },
        trait_invoke: struct {
            target: *Node, // expected to be a trait_cast node
            method: []const u8,
            args: std.ArrayList(*Node),
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
            .unary_expr => node.data = .{ .unary_expr = .{
                .operator = undefined,
                .right = undefined,
            } },
            .logical_expr => node.data = .{ .logical_expr = .{
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
            .expression_stmt => node.data = .{ .expression_stmt = .{
                .expr = undefined,
            } },
            .if_stmt => node.data = .{ .if_stmt = .{
                .condition = undefined,
                .then_branch = undefined,
                .else_branch = null,
            } },
            .while_stmt => node.data = .{ .while_stmt = .{
                .condition = undefined,
                .body = undefined,
            } },
            .for_stmt => node.data = .{ .for_stmt = .{
                .initializer = null,
                .condition = null,
                .increment = null,
                .body = undefined,
            } },
            .return_stmt => node.data = .{ .return_stmt = .{
                .value = null,
            } },
            .break_stmt => node.data = .{ .break_stmt = .{} },
            .continue_stmt => node.data = .{ .continue_stmt = .{} },
            .member_expr => node.data = .{ .member_expr = .{
                .object = undefined,
                .field = "",
            } },
            .member_assign => node.data = .{ .member_assign = .{
                .object = undefined,
                .field = "",
                .value = undefined,
            } },
            .struct_lit => node.data = .{ .struct_lit = .{
                .type_name = "",
                .inits = std.ArrayList(FieldInit).init(allocator),
            } },
            .struct_decl => node.data = .{ .struct_decl = .{
                .name = "",
                .fields = std.ArrayList([]const u8).init(allocator),
            } },
            .trait_decl => node.data = .{ .trait_decl = .{
                .name = "",
                .methods = std.ArrayList(MethodSig).init(allocator),
            } },
            .impl_decl => node.data = .{ .impl_decl = .{
                .trait_name = "",
                .type_name = "",
                .methods = std.ArrayList(MethodImpl).init(allocator),
            } },
            .trait_cast => node.data = .{ .trait_cast = .{
                .object = undefined,
                .trait_name = "",
            } },
            .trait_invoke => node.data = .{ .trait_invoke = .{
                .target = undefined,
                .method = "",
                .args = std.ArrayList(*Node).init(allocator),
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
            .unary_expr => {
                self.data.unary_expr.right.deinit(allocator);
            },
            .logical_expr => {
                self.data.logical_expr.left.deinit(allocator);
                self.data.logical_expr.right.deinit(allocator);
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
            .expression_stmt => {
                self.data.expression_stmt.expr.deinit(allocator);
            },
            .if_stmt => {
                self.data.if_stmt.condition.deinit(allocator);
                self.data.if_stmt.then_branch.deinit(allocator);
                if (self.data.if_stmt.else_branch) |else_b| else_b.deinit(allocator);
            },
            .while_stmt => {
                self.data.while_stmt.condition.deinit(allocator);
                self.data.while_stmt.body.deinit(allocator);
            },
            .for_stmt => {
                if (self.data.for_stmt.initializer) |init_node| init_node.deinit(allocator);
                if (self.data.for_stmt.condition) |cond| cond.deinit(allocator);
                if (self.data.for_stmt.increment) |inc| inc.deinit(allocator);
                self.data.for_stmt.body.deinit(allocator);
            },
            .return_stmt => {
                if (self.data.return_stmt.value) |val| val.deinit(allocator);
            },
            .break_stmt => {},
            .continue_stmt => {},
            .member_expr => {
                self.data.member_expr.object.deinit(allocator);
                allocator.free(self.data.member_expr.field);
            },
            .member_assign => {
                self.data.member_assign.object.deinit(allocator);
                self.data.member_assign.value.deinit(allocator);
                allocator.free(self.data.member_assign.field);
            },
            .struct_lit => {
                for (self.data.struct_lit.inits.items) |fi| {
                    allocator.free(fi.name);
                    fi.expr.deinit(allocator);
                }
                self.data.struct_lit.inits.deinit();
                allocator.free(self.data.struct_lit.type_name);
            },
            .struct_decl => {
                for (self.data.struct_decl.fields.items) |fname| {
                    allocator.free(fname);
                }
                self.data.struct_decl.fields.deinit();
                allocator.free(self.data.struct_decl.name);
            },
            .trait_decl => {
                allocator.free(self.data.trait_decl.name);
                for (self.data.trait_decl.methods.items) |method| {
                    allocator.free(method.name);
                    method.params.deinit();
                }
                self.data.trait_decl.methods.deinit();
            },
            .impl_decl => {
                allocator.free(self.data.impl_decl.trait_name);
                allocator.free(self.data.impl_decl.type_name);
                for (self.data.impl_decl.methods.items) |method| {
                    allocator.free(method.name);
                    method.params.deinit();
                    method.body.deinit(allocator);
                }
                self.data.impl_decl.methods.deinit();
            },
            .trait_cast => {
                self.data.trait_cast.object.deinit(allocator);
                allocator.free(self.data.trait_cast.trait_name);
            },
            .trait_invoke => {
                self.data.trait_invoke.target.deinit(allocator);
                for (self.data.trait_invoke.args.items) |arg| {
                    arg.deinit(allocator);
                }
                self.data.trait_invoke.args.deinit();
                allocator.free(self.data.trait_invoke.method);
            },
        }
        allocator.destroy(self);
    }
};
