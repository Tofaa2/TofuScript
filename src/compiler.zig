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
    locals: std.ArrayList(Local),
    upvalues: std.ArrayList(Upvalue),
    struct_types: std.StringHashMap(*value.ObjStructType),
    trait_types: std.StringHashMap(*value.ObjTraitType),
    impls: std.StringHashMap(std.StringHashMap(std.ArrayList(*ObjFunction))),

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

    const Local = struct {
        name: []const u8,
        depth: i32, // -1 means uninitialized (declared but not defined)
    };

    const Upvalue = struct {
        index: u8,
        is_local: bool,
    };

    pub fn init(allocator: std.mem.Allocator, function_type: FunctionType) !*Compiler {
        const compiler = try allocator.create(Compiler);
        compiler.* = .{
            .allocator = allocator,
            .function = try ObjFunction.init(allocator),
            .function_type = function_type,
            .loop_stack = std.ArrayList(LoopCtx).init(allocator),
            .locals = std.ArrayList(Local).init(allocator),
            .upvalues = std.ArrayList(Upvalue).init(allocator),
            .struct_types = std.StringHashMap(*value.ObjStructType).init(allocator),
            .trait_types = std.StringHashMap(*value.ObjTraitType).init(allocator),
            .impls = std.StringHashMap(std.StringHashMap(std.ArrayList(*ObjFunction))).init(allocator),
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

        // Free local names and list
        for (self.locals.items) |loc| {
            self.allocator.free(loc.name);
        }
        self.locals.deinit();

        self.upvalues.deinit();

        // Free struct types
        var iter = self.struct_types.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.*.deinit(self.allocator);
        }
        self.struct_types.deinit();

        // Free trait types
        var trait_iter = self.trait_types.iterator();
        while (trait_iter.next()) |entry| {
            entry.value_ptr.*.deinit(self.allocator);
        }
        self.trait_types.deinit();

        // Free impls
        var impl_iter = self.impls.iterator();
        while (impl_iter.next()) |entry| {
            var trait_map = entry.value_ptr.*;
            var trait_iter2 = trait_map.iterator();
            while (trait_iter2.next()) |entry2| {
                entry2.value_ptr.deinit();
            }
            trait_map.deinit();
        }
        self.impls.deinit();

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
                for (node.data.function_decl.params.items) |param_name| {
                    compiler.function.arity += 1;
                    try compiler.declareVariable(param_name);
                    try compiler.defineVariable(param_name);
                }
                try compiler.compile(node.data.function_decl.body);
                try compiler.emitReturn();

                // Finalize function upvalue count
                compiler.function.upvalue_count = @intCast(compiler.upvalues.items.len);

                // Reference the function object via constant
                const function = compiler.function;
                const func_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &function.obj });
                if (func_index > 255) return error.TooManyConstants;

                // Emit OP_CLOSURE func_index and encoded upvalues (pushes closure on stack)
                try self.emitByte(@intFromEnum(OpCode.closure));
                try self.emitByte(@intCast(func_index));
                for (compiler.upvalues.items) |uv| {
                    try self.emitByte(if (uv.is_local) 1 else 0);
                    try self.emitByte(uv.index);
                }

                // Bind the closure to a name in current scope
                if (self.scope_depth == 0) {
                    // Global: define_global pops the closure
                    try self.emitByte(@intFromEnum(OpCode.define_global));
                    const name_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &name.obj });
                    if (name_index > 255) return error.TooManyConstants;
                    try self.emitByte(@intCast(name_index));
                } else {
                    // Local: declare/define the local; value remains on stack as the local slot
                    try self.declareVariable(node.data.function_decl.name);
                    try self.defineVariable(node.data.function_decl.name);
                }
            },
            .variable_decl => {
                const name = node.data.variable_decl.name;

                // Evaluate initializer or push nil so value is on the stack
                if (node.data.variable_decl.value) |value_expr| {
                    try self.compile(value_expr);
                } else {
                    try self.emitByte(@intFromEnum(OpCode.nil));
                }

                if (self.scope_depth == 0) {
                    // Global variable
                    try self.emitByte(@intFromEnum(OpCode.define_global));
                    const name_obj = try ObjString.init(self.allocator, name);
                    const name_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &name_obj.obj });
                    if (name_index > 255) return error.TooManyConstants;
                    try self.emitByte(@intCast(name_index));
                } else {
                    // Local variable: just bind it to the current scope; value already on stack
                    try self.declareVariable(name);
                    try self.defineVariable(name);
                }
            },
            .assignment => {
                const name = node.data.assignment.name;
                try self.compile(node.data.assignment.value);

                if (self.resolveLocal(name)) |local_index| {
                    try self.emitByte(@intFromEnum(OpCode.store_local));
                    try self.emitByte(local_index);
                } else if (self.resolveUpvalue(name)) |uv_index| {
                    try self.emitByte(@intFromEnum(OpCode.set_upvalue));
                    try self.emitByte(uv_index);
                } else {
                    // set_global name
                    try self.emitByte(@intFromEnum(OpCode.set_global));
                    const assign_name_obj = try ObjString.init(self.allocator, name);
                    const assign_name_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &assign_name_obj.obj });
                    if (assign_name_index > 255) return error.TooManyConstants;
                    try self.emitByte(@intCast(assign_name_index));
                }
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
                if (self.resolveLocal(name)) |local_index| {
                    try self.emitByte(@intFromEnum(OpCode.load_local));
                    try self.emitByte(local_index);
                } else if (self.resolveUpvalue(name)) |uv_index| {
                    try self.emitByte(@intFromEnum(OpCode.get_upvalue));
                    try self.emitByte(uv_index);
                } else {
                    try self.emitByte(@intFromEnum(OpCode.get_global));
                    const ident_name_obj = try ObjString.init(self.allocator, name);
                    const ident_name_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &ident_name_obj.obj });
                    if (ident_name_index > 255) return error.TooManyConstants;
                    try self.emitByte(@intCast(ident_name_index));
                }
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
            .struct_decl => {
                const name = node.data.struct_decl.name;
                const fields = node.data.struct_decl.fields.items;
                const ty = try value.ObjStructType.init(self.allocator, name, fields);
                try self.struct_types.put(try self.allocator.dupe(u8, name), ty);
                // Store in globals
                const ty_val = Value{ .obj = &ty.obj };
                // push type object
                try self.emitConstant(ty_val);
                // define as global with name
                try self.emitByte(@intFromEnum(OpCode.define_global));
                const name_obj = try ObjString.init(self.allocator, name);
                const name_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &name_obj.obj });
                if (name_index > 255) return error.TooManyConstants;
                try self.emitByte(@intCast(name_index));
            },
            .member_expr => {
                try self.compile(node.data.member_expr.object);
                const field_name = node.data.member_expr.field;
                const field_name_obj = try ObjString.init(self.allocator, field_name);
                const field_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &field_name_obj.obj });
                if (field_index > 255) return error.TooManyConstants;
                try self.emitByte(@intFromEnum(OpCode.get_field));
                try self.emitByte(@intCast(field_index));
            },
            .member_assign => {
                // VM expects stack as [ ..., instance, value ] before OP_SET_FIELD
                try self.compile(node.data.member_assign.object);
                try self.compile(node.data.member_assign.value);
                const field_name = node.data.member_assign.field;
                const field_name_obj = try ObjString.init(self.allocator, field_name);
                const field_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &field_name_obj.obj });
                if (field_index > 255) return error.TooManyConstants;
                try self.emitByte(@intFromEnum(OpCode.set_field));
                try self.emitByte(@intCast(field_index));
            },
            .struct_lit => {
                const type_name = node.data.struct_lit.type_name;
                // Get the type from globals
                const type_name_obj = try ObjString.init(self.allocator, type_name);
                const type_name_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &type_name_obj.obj });
                if (type_name_index > 255) return error.TooManyConstants;
                try self.emitByte(@intFromEnum(OpCode.get_global));
                try self.emitByte(@intCast(type_name_index));
                // Now type is on stack
                try self.emitByte(@intFromEnum(OpCode.new_instance));
                // For each field init
                for (node.data.struct_lit.inits.items) |fi| {
                    // Dup instance so we can set a field without losing the original
                    try self.emitByte(@intFromEnum(OpCode.dup));
                    // Compile value
                    try self.compile(fi.expr);
                    // Set field (stack expects [ ..., instance, value ])
                    const field_name_obj = try ObjString.init(self.allocator, fi.name);
                    const field_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &field_name_obj.obj });
                    if (field_index > 255) return error.TooManyConstants;
                    try self.emitByte(@intFromEnum(OpCode.set_field));
                    try self.emitByte(@intCast(field_index));
                    // set_field pushes the assigned value; pop it to keep the instance on top for subsequent fields
                    try self.emitByte(@intFromEnum(OpCode.pop));
                }
            },
            .trait_decl => {
                const name = node.data.trait_decl.name;
                var methods = std.ArrayList([]const u8).init(self.allocator);
                defer methods.deinit();
                for (node.data.trait_decl.methods.items) |method| {
                    try methods.append(method.name);
                }
                const ty = try value.ObjTraitType.init(self.allocator, name, methods.items);
                try self.trait_types.put(try self.allocator.dupe(u8, name), ty);
                // Store in globals
                const ty_val = Value{ .obj = &ty.obj };
                try self.emitConstant(ty_val);
                try self.emitByte(@intFromEnum(OpCode.define_global));
                const name_obj = try ObjString.init(self.allocator, name);
                const name_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &name_obj.obj });
                if (name_index > 255) return error.TooManyConstants;
                try self.emitByte(@intCast(name_index));
            },
            .impl_decl => {
                const trait_name = node.data.impl_decl.trait_name;
                const type_name = node.data.impl_decl.type_name;
                const methods = node.data.impl_decl.methods;

                // Get or create the trait map
                var trait_map = self.impls.get(type_name) orelse blk: {
                    const new_map = std.StringHashMap(std.ArrayList(*ObjFunction)).init(self.allocator);
                    try self.impls.put(try self.allocator.dupe(u8, type_name), new_map);
                    break :blk new_map;
                };

                var func_list = std.ArrayList(*ObjFunction).init(self.allocator);
                for (methods.items) |method| {
                    // Compile method as function
                    const method_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}_{s}", .{ type_name, trait_name, method.name });
                    defer self.allocator.free(method_name);

                    var compiler = try Compiler.init(self.allocator, .function);
                    compiler.enclosing = self;
                    const func_name_obj = try ObjString.init(self.allocator, method_name);
                    compiler.function.name = func_name_obj;

                    // Add self parameter first (implicit)
                    compiler.beginScope();
                    try compiler.declareVariable("self");
                    try compiler.defineVariable("self");
                    compiler.function.arity = 1; // count 'self'

                    // Add other params (skip explicit 'self' if present in signature)
                    for (method.params.items) |param| {
                        if (!std.mem.eql(u8, param, "self")) {
                            try compiler.declareVariable(param);
                            try compiler.defineVariable(param);
                            compiler.function.arity += 1;
                        }
                    }

                    try compiler.compile(method.body);
                    try compiler.emitReturn();

                    compiler.function.upvalue_count = @intCast(compiler.upvalues.items.len);

                    // Debug: disassemble compiled trait method when tracing enabled
                    if (trace.enabled) {
                        compiler.function.chunk.disassemble(method_name);
                    }

                    try func_list.append(compiler.function);

                    // Define as global
                    try self.emitConstant(Value{ .obj = &compiler.function.obj });
                    try self.emitByte(@intFromEnum(OpCode.define_global));
                    const name_obj = try ObjString.init(self.allocator, method_name);
                    const name_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &name_obj.obj });
                    if (name_index > 255) return error.TooManyConstants;
                    try self.emitByte(@intCast(name_index));

                    // Do not deinit nested compiler here; function object must remain alive.
                    // compiler.deinit();
                }

                try trait_map.put(try self.allocator.dupe(u8, trait_name), func_list);
            },
            .trait_cast => {
                // Stack contract for OP_CAST_TO_TRAIT (in VM):
                // expects [ ... , trait_type, instance ] then pops both and pushes trait_instance.
                // Emit: GET_GLOBAL trait_name; then compile object; then OP_CAST_TO_TRAIT.
                const trait_name = node.data.trait_cast.trait_name;
                try self.emitByte(@intFromEnum(OpCode.get_global));
                const name_obj = try ObjString.init(self.allocator, trait_name);
                const name_index: u24 = try self.function.chunk.addConstant(Value{ .obj = &name_obj.obj });
                if (name_index > 255) return error.TooManyConstants;
                try self.emitByte(@intCast(name_index));

                try self.compile(node.data.trait_cast.object);

                try self.emitByte(@intFromEnum(OpCode.cast_to_trait));
            },
            .trait_invoke => {
                // Compile target (should evaluate to a trait_instance on stack),
                // then push args, then emit OP_INVOKE_TRAIT method_idx, argc.
                try self.compile(node.data.trait_invoke.target);

                // Determine method index from the trait referenced in the target cast
                const target_cast = node.data.trait_invoke.target.data.trait_cast;
                const trait_name = target_cast.trait_name;

                const trait_ty_ptr = self.getTraitType(trait_name) orelse return error.TraitNotFound;
                const method_idx_usize = trait_ty_ptr.methodIndex(node.data.trait_invoke.method) orelse return error.MethodNotFound;
                if (method_idx_usize > std.math.maxInt(u8)) return error.TooManyMethods;
                const method_idx: u8 = @intCast(method_idx_usize);

                // Compile arguments
                for (node.data.trait_invoke.args.items) |arg| {
                    try self.compile(arg);
                }
                const argc: u8 = @intCast(node.data.trait_invoke.args.items.len);

                try self.emitByte(@intFromEnum(OpCode.invoke_trait));
                try self.emitByte(method_idx);
                try self.emitByte(argc);
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

    fn declareVariable(self: *Compiler, name: []const u8) !void {
        if (self.scope_depth == 0) return;

        // Check for duplicate variable in the current scope
        var i: isize = @as(isize, @intCast(self.locals.items.len)) - 1;
        while (i >= 0) : (i -= 1) {
            const local = self.locals.items[@intCast(i)];
            if (local.depth != -1 and local.depth < self.scope_depth) break;
            if (std.mem.eql(u8, local.name, name)) {
                return error.VariableAlreadyDeclared;
            }
        }

        const dup = try self.allocator.dupe(u8, name);
        try self.locals.append(.{ .name = dup, .depth = -1 });
    }

    fn defineVariable(self: *Compiler, _: []const u8) !void {
        if (self.scope_depth == 0) return;
        self.locals.items[self.locals.items.len - 1].depth = self.scope_depth;
    }

    fn resolveLocal(self: *Compiler, name: []const u8) ?u8 {
        var i: isize = @as(isize, @intCast(self.locals.items.len)) - 1;
        while (i >= 0) : (i -= 1) {
            const local = self.locals.items[@intCast(i)];
            if (std.mem.eql(u8, local.name, name)) {
                const idx: usize = @intCast(i);
                if (idx > std.math.maxInt(u8)) return null;
                return @intCast(idx);
            }
        }
        return null;
    }

    fn addUpvalue(self: *Compiler, index: u8, is_local: bool) !u8 {
        // Deduplicate
        var i: usize = 0;
        while (i < self.upvalues.items.len) : (i += 1) {
            const uv = self.upvalues.items[i];
            if (uv.index == index and uv.is_local == is_local) return @intCast(i);
        }
        try self.upvalues.append(.{ .index = index, .is_local = is_local });
        if (self.upvalues.items.len - 1 > std.math.maxInt(u8)) return error.TooManyUpvalues;
        return @intCast(self.upvalues.items.len - 1);
    }

    fn resolveUpvalue(self: *Compiler, name: []const u8) ?u8 {
        if (self.enclosing) |enc| {
            if (enc.resolveLocal(name)) |local_idx| {
                return self.addUpvalue(local_idx, true) catch null;
            }
            if (enc.resolveUpvalue(name)) |up_idx| {
                return self.addUpvalue(up_idx, false) catch null;
            }
        }
        return null;
    }

    fn getTraitType(self: *Compiler, name: []const u8) ?*value.ObjTraitType {
        var c: ?*Compiler = self;
        while (c) |comp| {
            if (comp.trait_types.get(name)) |tt| return tt;
            c = comp.enclosing;
        }
        return null;
    }

    fn beginScope(self: *Compiler) void {
        self.scope_depth += 1;
    }

    fn endScope(self: *Compiler) !void {
        self.scope_depth -= 1;
        // Pop locals belonging to this scope
        while (self.locals.items.len > 0 and self.locals.items[self.locals.items.len - 1].depth > self.scope_depth) {
            try self.emitByte(@intFromEnum(OpCode.pop));
            const last = self.locals.pop().?;
            self.allocator.free(last.name);
        }
    }
};
