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
    loop_stack: std.ArrayList(LoopCtx),

    const FunctionType = enum {
        function,
        script,
    };

    const LoopCtx = struct {
        start: usize,
        continue_patches: std.ArrayList(usize),
        break_patches: std.ArrayList(usize),
        continue_target: ?usize = null,
    };

    pub fn init(allocator: std.mem.Allocator, function_type: FunctionType) !*Compiler {
        const compiler = try allocator.create(Compiler);
        compiler.* = .{
            .allocator = allocator,
            .function = try ObjFunction.init(allocator),
            .function_type = function_type,
            .loop_stack = std.ArrayList(LoopCtx).init(allocator),
        };
        return compiler;
    }

    pub fn deinit(self: *Compiler) void {
        // Clean up any remaining loop contexts (should be empty)
        for (self.loop_stack.items) |*ctx| {
            ctx.continue_patches.deinit();
            ctx.break_patches.deinit();
        }
        self.loop_stack.deinit();

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

                // Emit define_global with an immediate constant index operand
                try self.emitByte(@intFromEnum(OpCode.define_global));
                const name_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &name.obj });
                if (name_index > 255) return error.TooManyConstants;
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

                // define_global name
                try self.emitByte(@intFromEnum(OpCode.define_global));
                const name_obj = try ObjString.init(self.allocator, name);
                const name_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &name_obj.obj });
                if (name_index > 255) return error.TooManyConstants;
                try self.emitByte(@intCast(name_index));
            },
            .assignment => {
                try self.compile(node.data.assignment.value);

                // set_global name
                try self.emitByte(@intFromEnum(OpCode.set_global));
                const assign_name_obj = try ObjString.init(self.allocator, node.data.assignment.name);
                const assign_name_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &assign_name_obj.obj });
                if (assign_name_index > 255) return error.TooManyConstants;
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
                } else if (std.mem.eql(u8, operator, "!=")) {
                    try self.emitByte(@intFromEnum(OpCode.equal));
                    try self.emitByte(@intFromEnum(OpCode.not));
                } else if (std.mem.eql(u8, operator, ">")) {
                    try self.emitByte(@intFromEnum(OpCode.greater));
                } else if (std.mem.eql(u8, operator, ">=")) {
                    // >= is !(a < b)
                    try self.emitByte(@intFromEnum(OpCode.less));
                    try self.emitByte(@intFromEnum(OpCode.not));
                } else if (std.mem.eql(u8, operator, "<")) {
                    try self.emitByte(@intFromEnum(OpCode.less));
                } else if (std.mem.eql(u8, operator, "<=")) {
                    // <= is !(a > b)
                    try self.emitByte(@intFromEnum(OpCode.greater));
                    try self.emitByte(@intFromEnum(OpCode.not));
                } else {
                    return error.UnknownOperator;
                }
            },
            .unary_expr => {
                try self.compile(node.data.unary_expr.right);
                const op_lex = node.data.unary_expr.operator.lexeme;
                if (std.mem.eql(u8, op_lex, "!")) {
                    try self.emitByte(@intFromEnum(OpCode.not));
                } else if (std.mem.eql(u8, op_lex, "-")) {
                    try self.emitByte(@intFromEnum(OpCode.negate));
                } else {
                    return error.UnknownOperator;
                }
            },
            .logical_expr => {
                // Short-circuiting for 'and' and 'or'
                if (node.data.logical_expr.operator.type == .@"and") {
                    try self.compile(node.data.logical_expr.left);
                    const end_jump = try self.emitJump(OpCode.jump_if_false);
                    try self.emitByte(@intFromEnum(OpCode.pop));
                    try self.compile(node.data.logical_expr.right);
                    try self.patchJump(end_jump);
                } else {
                    // or
                    try self.compile(node.data.logical_expr.left);
                    const right_jump = try self.emitJump(OpCode.jump_if_false);
                    const end_jump = try self.emitJump(OpCode.jump);
                    try self.patchJump(right_jump);
                    try self.emitByte(@intFromEnum(OpCode.pop));
                    try self.compile(node.data.logical_expr.right);
                    try self.patchJump(end_jump);
                }
            },
            .expression_stmt => {
                try self.compile(node.data.expression_stmt.expr);
                try self.emitByte(@intFromEnum(OpCode.pop));
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
                try self.emitByte(@intFromEnum(OpCode.get_global));
                const ident_name_obj = try ObjString.init(self.allocator, name);
                const ident_name_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &ident_name_obj.obj });
                if (ident_name_index > 255) return error.TooManyConstants;
                try self.emitByte(@intCast(ident_name_index));
            },
            .call_expr => {
                try self.compile(node.data.call_expr.callee);
                for (node.data.call_expr.args.items) |arg| {
                    try self.compile(arg);
                }
                try self.emitByte(@intFromEnum(OpCode.call));
                try self.emitByte(@intCast(node.data.call_expr.args.items.len));
            },
            .import_stmt => {
                trace.log("COMP", "import module={s}", .{node.data.import_stmt.module});
            },
            .block_stmt => {
                self.beginScope();
                for (node.data.block_stmt.statements.items) |stmt| {
                    try self.compile(stmt);
                }
                try self.endScope();
            },
            .if_stmt => {
                try self.compile(node.data.if_stmt.condition);
                const then_jump = try self.emitJump(OpCode.jump_if_false);
                try self.emitByte(@intFromEnum(OpCode.pop)); // pop condition for then branch

                try self.compile(node.data.if_stmt.then_branch);

                const end_jump = try self.emitJump(OpCode.jump);
                try self.patchJump(then_jump);
                try self.emitByte(@intFromEnum(OpCode.pop)); // pop condition for else path

                if (node.data.if_stmt.else_branch) |else_b| {
                    try self.compile(else_b);
                }
                try self.patchJump(end_jump);
            },
            .while_stmt => {
                const loop_start = self.currentOffset();

                try self.compile(node.data.while_stmt.condition);
                const exit_jump = try self.emitJump(OpCode.jump_if_false);
                try self.emitByte(@intFromEnum(OpCode.pop)); // pop condition before body

                // Push loop context
                const ctx = LoopCtx{
                    .start = loop_start,
                    .continue_patches = std.ArrayList(usize).init(self.allocator),
                    .break_patches = std.ArrayList(usize).init(self.allocator),
                    .continue_target = null,
                };
                try self.loop_stack.append(ctx);

                try self.compile(node.data.while_stmt.body);

                // Mark continue target just before looping back
                var ctx_ref = &self.loop_stack.items[self.loop_stack.items.len - 1];
                ctx_ref.continue_target = self.currentOffset();
                // Patch any continue jumps to this target
                for (ctx_ref.continue_patches.items) |off| {
                    try self.patchJumpTo(off, ctx_ref.continue_target.?);
                }
                ctx_ref.continue_patches.clearRetainingCapacity();

                try self.emitLoop(loop_start);

                try self.patchJump(exit_jump);
                try self.emitByte(@intFromEnum(OpCode.pop)); // pop condition on exit

                // Patch breaks to here
                for (ctx_ref.break_patches.items) |off| {
                    try self.patchJumpTo(off, self.currentOffset());
                }

                // Cleanup loop context
                ctx_ref.continue_patches.deinit();
                ctx_ref.break_patches.deinit();
                _ = self.loop_stack.pop();
            },
            .for_stmt => {
                // initializer
                if (node.data.for_stmt.initializer) |init_node| {
                    try self.compile(init_node);
                }

                const loop_start = self.currentOffset();

                var exit_jump: ?usize = null;
                if (node.data.for_stmt.condition) |cond| {
                    try self.compile(cond);
                    exit_jump = try self.emitJump(OpCode.jump_if_false);
                    try self.emitByte(@intFromEnum(OpCode.pop)); // pop condition before body
                }

                // Push loop context
                const ctx = LoopCtx{
                    .start = loop_start,
                    .continue_patches = std.ArrayList(usize).init(self.allocator),
                    .break_patches = std.ArrayList(usize).init(self.allocator),
                    .continue_target = null,
                };
                try self.loop_stack.append(ctx);

                // body
                try self.compile(node.data.for_stmt.body);

                // continue target label (start of increment if present)
                var ctx_ref = &self.loop_stack.items[self.loop_stack.items.len - 1];
                ctx_ref.continue_target = self.currentOffset();

                // increment
                if (node.data.for_stmt.increment) |inc| {
                    try self.compile(inc);
                    try self.emitByte(@intFromEnum(OpCode.pop));
                }

                // Patch continues to increment (or to this point if no increment)
                for (ctx_ref.continue_patches.items) |off| {
                    try self.patchJumpTo(off, ctx_ref.continue_target.?);
                }
                ctx_ref.continue_patches.clearRetainingCapacity();

                try self.emitLoop(loop_start);

                if (exit_jump) |ej| {
                    try self.patchJump(ej);
                    try self.emitByte(@intFromEnum(OpCode.pop)); // pop condition on loop exit
                }

                // Patch breaks to here
                for (ctx_ref.break_patches.items) |off| {
                    try self.patchJumpTo(off, self.currentOffset());
                }

                // Cleanup loop context
                ctx_ref.continue_patches.deinit();
                ctx_ref.break_patches.deinit();
                _ = self.loop_stack.pop();
            },
            .return_stmt => {
                if (node.data.return_stmt.value) |val| {
                    try self.compile(val);
                } else {
                    try self.emitByte(@intFromEnum(OpCode.nil));
                }
                try self.emitByte(@intFromEnum(OpCode.@"return"));
            },
            .break_stmt => {
                if (self.loop_stack.items.len == 0) return error.LoopControlOutsideLoop;
                const jmp_off = try self.emitJump(OpCode.jump);
                var ctx_ref = &self.loop_stack.items[self.loop_stack.items.len - 1];
                try ctx_ref.break_patches.append(jmp_off);
            },
            .continue_stmt => {
                if (self.loop_stack.items.len == 0) return error.LoopControlOutsideLoop;
                const jmp_off = try self.emitJump(OpCode.jump);
                var ctx_ref = &self.loop_stack.items[self.loop_stack.items.len - 1];
                try ctx_ref.continue_patches.append(jmp_off);
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

    fn currentOffset(self: *Compiler) usize {
        return self.function.chunk.code.items.len;
    }

    fn emitJump(self: *Compiler, opcode: OpCode) !usize {
        try self.emitByte(@intFromEnum(opcode));
        const operand_index = self.currentOffset();
        // Two-byte placeholder for u16 jump offset
        try self.emitByte(0);
        try self.emitByte(0);
        return operand_index;
    }

    fn patchJump(self: *Compiler, operand_index: usize) !void {
        const jump = self.currentOffset() - operand_index - 2;
        if (jump > std.math.maxInt(u16)) return error.TooMuchJump;
        const j: u16 = @intCast(jump);
        self.function.chunk.code.items[operand_index] = @intCast(j & 0xFF);
        self.function.chunk.code.items[operand_index + 1] = @intCast((j >> 8) & 0xFF);
    }

    fn patchJumpTo(self: *Compiler, operand_index: usize, target_offset: usize) !void {
        const jump = target_offset - operand_index - 2;
        if (jump > std.math.maxInt(u16)) return error.TooMuchJump;
        const j: u16 = @intCast(jump);
        self.function.chunk.code.items[operand_index] = @intCast(j & 0xFF);
        self.function.chunk.code.items[operand_index + 1] = @intCast((j >> 8) & 0xFF);
    }

    fn emitLoop(self: *Compiler, loop_start: usize) !void {
        // Jump back to loop_start
        try self.emitByte(@intFromEnum(OpCode.loop));
        // After reading the two bytes, ip will be at currentOffset()+2
        const after_operands = self.currentOffset() + 2;
        const jump_back = after_operands - loop_start;
        if (jump_back > std.math.maxInt(u16)) return error.LoopTooLarge;
        const j: u16 = @intCast(jump_back);
        try self.emitByte(@intCast(j & 0xFF));
        try self.emitByte(@intCast((j >> 8) & 0xFF));
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
