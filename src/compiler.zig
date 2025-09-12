const std = @import("std");
const ast = @import("ast.zig");
const bytecode = @import("bytecode.zig");
const value = @import("value.zig");
const trace = @import("trace.zig");

const Node = ast.Node;
const NodeType = ast.NodeType;
const Chunk = bytecode.Chunk;
const OpCode = bytecode.OpCode;
const Value = value.Value;
const ObjString = value.ObjString;
const ObjFunction = value.ObjFunction;

pub const Compiler = struct {
    allocator: std.mem.Allocator,
    enclosing: ?*Compiler = null,
    function: *ObjFunction,
    function_type: FunctionType = .script,
    scope_depth: i32 = 0,

    const FunctionType = enum {
        function,
        script,
    };

    pub fn init(allocator: std.mem.Allocator, function_type: FunctionType) !*Compiler {
        const compiler = try allocator.create(Compiler);
        compiler.* = .{
            .allocator = allocator,
            .function = try ObjFunction.init(allocator),
            .function_type = function_type,
        };
        return compiler;
    }

    pub fn deinit(self: *Compiler) void {
        self.function.deinit(self.allocator);
        self.allocator.destroy(self);
    }

    pub fn compile(self: *Compiler, node: *Node) !void {
        switch (node.type) {
            .program => {
                for (node.data.program.body.items) |stmt| {
                    try self.compile(stmt);
                }
                try self.emitReturn();
            },
            .function_decl => {
                const name = try ObjString.init(self.allocator, node.data.function_decl.name);

                var compiler = try Compiler.init(self.allocator, .function);
                compiler.enclosing = self;
                compiler.function.name = name;

                // Compile function body
                compiler.beginScope();
                for (node.data.function_decl.params.items) |_| {
                    compiler.function.arity += 1;
                    // TODO: Add parameter to local variables
                }
                try compiler.compile(node.data.function_decl.body);
                try compiler.emitReturn();

                // Create function object
                const function = compiler.function;

                // Push the function object onto the stack (define_global will pop it)
                try self.emitConstant(Value{ .obj = &function.obj });

                // Emit define_global with an immediate constant index operand (NOT a separate constant instruction)
                try self.emitByte(@intFromEnum(OpCode.define_global));
                const name_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &name.obj });
                if (name_index > 255) {
                    return error.TooManyConstants;
                }
                try self.emitByte(@intCast(name_index));
            },
            .variable_decl => {
                const name = node.data.variable_decl.name;

                // Evaluate initializer or push nil so value is on the stack
                if (node.data.variable_decl.value) |value_expr| {
                    try self.compile(value_expr);
                } else {
                    try self.emitByte(@intFromEnum(OpCode.nil));
                }

                // Emit define_global with immediate operand index of the name constant
                try self.emitByte(@intFromEnum(OpCode.define_global));
                const name_obj = try ObjString.init(self.allocator, name);
                const name_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &name_obj.obj });
                if (name_index > 255) {
                    return error.TooManyConstants;
                }
                try self.emitByte(@intCast(name_index));
            },
            .assignment => {
                // Value to assign left on the stack
                try self.compile(node.data.assignment.value);

                // set_global expects an immediate constant index operand for the variable name
                try self.emitByte(@intFromEnum(OpCode.set_global));
                const assign_name_obj = try ObjString.init(self.allocator, node.data.assignment.name);
                const assign_name_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &assign_name_obj.obj });
                if (assign_name_index > 255) {
                    return error.TooManyConstants;
                }
                try self.emitByte(@intCast(assign_name_index));
            },
            .binary_expr => {
                try self.compile(node.data.binary_expr.left);
                try self.compile(node.data.binary_expr.right);

                const operator = node.data.binary_expr.operator.lexeme;
                if (std.mem.eql(u8, operator, "+")) {
                    try self.emitByte(@intFromEnum(OpCode.add));
                } else if (std.mem.eql(u8, operator, "-")) {
                    try self.emitByte(@intFromEnum(OpCode.subtract));
                } else if (std.mem.eql(u8, operator, "*")) {
                    try self.emitByte(@intFromEnum(OpCode.multiply));
                } else if (std.mem.eql(u8, operator, "/")) {
                    try self.emitByte(@intFromEnum(OpCode.divide));
                } else if (std.mem.eql(u8, operator, "==")) {
                    try self.emitByte(@intFromEnum(OpCode.equal));
                } else if (std.mem.eql(u8, operator, ">")) {
                    try self.emitByte(@intFromEnum(OpCode.greater));
                } else if (std.mem.eql(u8, operator, "<")) {
                    try self.emitByte(@intFromEnum(OpCode.less));
                } else {
                    return error.UnknownOperator;
                }
            },
            .literal => {
                const value_lit = node.data.literal.value;
                try self.emitConstant(switch (value_lit) {
                    .number => |n| Value{ .number = n },
                    .string => |s| Value{ .obj = &(try ObjString.init(self.allocator, s)).obj },
                    .boolean => |b| if (b) Value{ .boolean = true } else Value{ .boolean = false },
                    .nil => Value{ .nil = {} },
                });
            },
            .identifier => {
                const name = node.data.identifier.name;

                // get_global expects an immediate constant index operand for the name
                try self.emitByte(@intFromEnum(OpCode.get_global));
                const ident_name_obj = try ObjString.init(self.allocator, name);
                const ident_name_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &ident_name_obj.obj });
                if (ident_name_index > 255) {
                    return error.TooManyConstants;
                }
                try self.emitByte(@intCast(ident_name_index));
            },
            .call_expr => {
                try self.compile(node.data.call_expr.callee);

                // Compile arguments
                for (node.data.call_expr.args.items) |arg| {
                    try self.compile(arg);
                }

                try self.emitByte(@intFromEnum(OpCode.call));
                try self.emitByte(@intCast(node.data.call_expr.args.items.len));
            },
            .import_stmt => {
                // For now, we'll just ignore imports
                // In a real implementation, we'd load and compile the imported module
                trace.log("COMP", "import module={s}", .{node.data.import_stmt.module});
            },
            .block_stmt => {
                self.beginScope();
                for (node.data.block_stmt.statements.items) |stmt| {
                    try self.compile(stmt);
                }
                try self.endScope();
            },
        }
    }

    fn emitByte(self: *Compiler, byte: u8) !void {
        try self.function.chunk.write(byte, 0); // TODO: Track line numbers
    }

    fn emitBytes(self: *Compiler, bytes: []const u8) !void {
        for (bytes) |byte| {
            try self.emitByte(byte);
        }
    }

    fn emitConstant(self: *Compiler, value_o: Value) !void {
        const constant = try self.function.chunk.addConstant(value_o);
        trace.log("COMP", "emit const index={d}", .{constant});

        if (constant <= 255) {
            trace.log("COMP", "const.short index={d}", .{constant});
            try self.emitBytes(&[_]u8{
                @intFromEnum(OpCode.constant),
                @intCast(constant),
            });
        } else {
            trace.log("COMP", "const.long bytes=[{d},{d},{d}]", .{ constant & 0xFF, (constant >> 8) & 0xFF, (constant >> 16) & 0xFF });
            try self.emitBytes(&[_]u8{
                @intFromEnum(OpCode.constant_long),
                @intCast(constant & 0xFF),
                @intCast((constant >> 8) & 0xFF),
                @intCast((constant >> 16) & 0xFF),
            });
        }
    }

    fn emitReturn(self: *Compiler) !void {
        if (self.function_type == .script) {
            try self.emitByte(@intFromEnum(OpCode.nil));
        } else {
            try self.emitByte(@intFromEnum(OpCode.nil));
        }
        try self.emitByte(@intFromEnum(OpCode.@"return"));
    }

    fn beginScope(self: *Compiler) void {
        self.scope_depth += 1;
    }

    fn endScope(self: *Compiler) !void {
        self.scope_depth -= 1;
        // TODO: Pop local variables
    }
};
