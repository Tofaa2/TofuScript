const std = @import("std");
const bytecode = @import("bytecode.zig");
const value = @import("value.zig");
const trace = @import("trace.zig");

const Chunk = bytecode.Chunk;
const OpCode = bytecode.OpCode;
const Value = value.Value;
const Obj = value.Obj;
const ObjFunction = value.ObjFunction;
const ObjClosure = value.ObjClosure;
const ObjNative = value.ObjNative;
const NativeFn = value.NativeFn;

pub const VM = struct {
    allocator: std.mem.Allocator,
    chunks: std.ArrayList(*Chunk),
    stack: std.ArrayList(Value),
    globals: std.StringHashMap(Value),
    frames: std.ArrayList(CallFrame),

    const CallFrame = struct {
        function: *ObjFunction,
        closure: ?*ObjClosure = null,
        ip: usize,
        slots: usize, // Stack offset for this frame
    };

    const StackMax = 256;
    const FramesMax = 64;

    pub fn init(allocator: std.mem.Allocator) !VM {
        return VM{
            .allocator = allocator,
            .chunks = std.ArrayList(*Chunk).init(allocator),
            .stack = std.ArrayList(Value).init(allocator),
            .globals = std.StringHashMap(Value).init(allocator),
            .frames = std.ArrayList(CallFrame).init(allocator),
        };
    }

    pub fn deinit(self: *VM) void {
        self.chunks.deinit();
        self.stack.deinit();

        var globals_iter = self.globals.iterator();
        while (globals_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.globals.deinit();

        self.frames.deinit();
    }

    pub fn interpret(self: *VM, function: *ObjFunction) !void {
        // Push the function object so top-level return can pop it, matching call frame convention.
        try self.push(Value{ .obj = &function.obj });
        try self.frames.append(.{
            .function = function,
            .closure = null,
            .ip = 0,
            .slots = self.stack.items.len - 1,
        });

        try self.run();
    }

    fn run(self: *VM) !void {
        var frame = &self.frames.items[self.frames.items.len - 1];

        // Add bounds checking
        if (frame.ip >= frame.function.chunk.code.items.len) {
            std.debug.print("ERROR: IP {d} exceeds code length {d}\n", .{ frame.ip, frame.function.chunk.code.items.len });
            return error.IPOutOfBounds;
        }

        while (true) {
            const instruction = self.readByte(frame);
            const instructionAsCode: OpCode = @enumFromInt(instruction);

            trace.log("VM", "ip={d} byte={d} opcode={s}", .{ frame.ip - 1, instruction, @tagName(instructionAsCode) });
            switch (instructionAsCode) {
                .constant => {
                    const constant = self.readByte(frame);
                    try self.push(frame.function.chunk.constants.items[constant]);
                },
                .constant_long => {
                    // const constant = @as(u24, self.readByte(frame)) |
                    //     (@as(u24, self.readByte(frame)) << 8) |
                    //     (@as(u24, self.readByte(frame)) << 16);
                    // try self.push(frame.function.chunk.constants.items[constant]);

                    const byte1 = self.readByte(frame);
                    const byte2 = self.readByte(frame);
                    const byte3 = self.readByte(frame);
                    const constant = @as(u24, byte1) | (@as(u24, byte2) << 8) | (@as(u24, byte3) << 16);

                    trace.log("VM", "constant_long bytes=[{d},{d},{d}] index={d} const_len={d}", .{ byte1, byte2, byte3, constant, frame.function.chunk.constants.items.len });

                    if (constant >= frame.function.chunk.constants.items.len) {
                        std.debug.print("ERROR: Constant index out of bounds!\n", .{});
                        return error.InvalidConstantIndex;
                    }

                    try self.push(frame.function.chunk.constants.items[constant]);
                },

                .get_global => {
                    const name_obj = self.readConstant(frame);
                    const name = @as(*value.ObjString, @fieldParentPtr("obj", name_obj.obj)).*;
                    if (trace.enabled) trace.log("VM", "get_global name={s}", .{name.chars});
                    if (self.globals.get(name.chars)) |value_i| {
                        if (trace.enabled) {
                            const tag = @as(std.meta.Tag(Value), value_i);
                            if (value_i == .obj) {
                                trace.log("VM", "get_global value tag={s} obj={s}", .{ @tagName(tag), @tagName(value_i.obj.type) });
                            } else {
                                trace.log("VM", "get_global value tag={s}", .{@tagName(tag)});
                            }
                        }
                        try self.push(value_i);
                    } else {
                        std.debug.print("Undefined variable: {s}\n", .{name.chars});
                        return error.UndefinedVariable;
                    }
                },
                .set_global => {
                    const name_obj = self.readConstant(frame);
                    const name = @as(*value.ObjString, @fieldParentPtr("obj", name_obj.obj)).*;
                    const value_i = self.peek(0);

                    if (self.globals.contains(name.chars)) {
                        _ = self.globals.put(name.chars, value_i) catch unreachable;
                    } else {
                        std.debug.print("Undefined variable: {s}\n", .{name.chars});
                        return error.UndefinedVariable;
                    }
                },
                .define_global => {
                    const name_obj = self.readConstant(frame);
                    const name = @as(*value.ObjString, @fieldParentPtr("obj", name_obj.obj)).*;
                    const value_i = self.pop();

                    try self.globals.put(try self.allocator.dupe(u8, name.chars), value_i);
                },
                .new_instance => {
                    const ty_val = self.pop();
                    if (ty_val != .obj or ty_val.obj.type != .struct_type) {
                        std.debug.print("OP_NEW_INSTANCE expects struct type\n", .{});
                        return error.BadInstanceType;
                    }
                    const ty = @as(*value.ObjStructType, @fieldParentPtr("obj", ty_val.obj));
                    const inst = try value.ObjInstance.init(self.allocator, ty);
                    try self.push(Value{ .obj = &inst.obj });
                },
                .get_field => {
                    const field_name_val = self.readConstant(frame);
                    const field_name = @as(*value.ObjString, @fieldParentPtr("obj", field_name_val.obj)).*;
                    const inst_val = self.pop();
                    if (inst_val != .obj or inst_val.obj.type != .instance) {
                        std.debug.print("OP_GET_FIELD expects instance\n", .{});
                        return error.BadFieldAccess;
                    }
                    const inst = @as(*value.ObjInstance, @fieldParentPtr("obj", inst_val.obj));
                    const field_idx = inst.ty.indexOf(field_name.chars) orelse {
                        std.debug.print("Field '{s}' not found\n", .{field_name.chars});
                        return error.FieldNotFound;
                    };
                    try self.push(inst.fields.items[field_idx]);
                },
                .set_field => {
                    const field_name_val = self.readConstant(frame);
                    const field_name = @as(*value.ObjString, @fieldParentPtr("obj", field_name_val.obj)).*;
                    const value_i = self.pop();
                    const inst_val = self.pop();
                    if (inst_val != .obj or inst_val.obj.type != .instance) {
                        std.debug.print("OP_SET_FIELD expects instance\n", .{});
                        return error.BadFieldAccess;
                    }
                    const inst = @as(*value.ObjInstance, @fieldParentPtr("obj", inst_val.obj));
                    const field_idx = inst.ty.indexOf(field_name.chars) orelse {
                        std.debug.print("Field '{s}' not found\n", .{field_name.chars});
                        return error.FieldNotFound;
                    };
                    inst.fields.items[field_idx] = value_i;
                    try self.push(value_i); // Assignment returns the value
                },
                .dup => {
                    try self.push(self.peek(0));
                },
                .cast_to_trait => {
                    const inst_val = self.pop();
                    const trait_ty_val = self.pop();
                    if (trait_ty_val != .obj or trait_ty_val.obj.type != .trait_type) {
                        std.debug.print("OP_CAST_TO_TRAIT expects trait type\n", .{});
                        return error.BadTraitType;
                    }
                    if (inst_val != .obj or inst_val.obj.type != .instance) {
                        std.debug.print("OP_CAST_TO_TRAIT expects instance\n", .{});
                        return error.BadInstanceType;
                    }
                    const trait_ty = @as(*value.ObjTraitType, @fieldParentPtr("obj", trait_ty_val.obj));
                    const inst = @as(*value.ObjInstance, @fieldParentPtr("obj", inst_val.obj));
                    const trait_inst = try value.ObjTraitInstance.init(self.allocator, trait_ty, inst);

                    // Set vtable
                    const type_name = inst.ty.name.chars;
                    const trait_name = trait_ty.name.chars;
                    for (trait_ty.method_names.items, 0..) |method_name, i| {
                        const func_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}_{s}", .{ type_name, trait_name, method_name });
                        defer self.allocator.free(func_name);

                        const func_name_obj = try value.ObjString.init(self.allocator, func_name);
                        defer func_name_obj.deinit(self.allocator);
                        if (self.globals.get(func_name_obj.chars)) |func_val| {
                            if (func_val != .obj or func_val.obj.type != .function) {
                                std.debug.print("Expected function for method {s}\n", .{method_name});
                                return error.BadMethodFunction;
                            }
                            const func = @as(*value.ObjFunction, @fieldParentPtr("obj", func_val.obj));
                            if (trace.enabled) trace.log("VM", "bind method {s} arity={d}", .{ method_name, func.arity });
                            const closure = try value.ObjClosure.init(self.allocator, func);
                            trait_inst.vtable.items[i] = Value{ .obj = &closure.obj };
                        } else {
                            std.debug.print("Method {s} not implemented\n", .{method_name});
                            return error.MethodNotImplemented;
                        }
                    }

                    try self.push(Value{ .obj = &trait_inst.obj });
                },
                .invoke_trait => {
                    const method_idx = self.readByte(frame);
                    const arg_count = self.readByte(frame);

                    // We want to transform the stack from:
                    //   [ ..., trait_instance, arg1, ..., argN ]
                    // to:
                    //   [ ..., closure, self, arg1, ..., argN ]
                    const base_index = self.stack.items.len - 1 - arg_count;

                    const trait_inst_val = self.stack.items[base_index];
                    if (trait_inst_val != .obj or trait_inst_val.obj.type != .trait_instance) {
                        std.debug.print("OP_INVOKE_TRAIT expects trait instance\n", .{});
                        return error.BadTraitInstance;
                    }
                    const trait_inst = @as(*value.ObjTraitInstance, @fieldParentPtr("obj", trait_inst_val.obj));

                    if (method_idx >= trait_inst.vtable.items.len) {
                        std.debug.print("Invalid method index {d}\n", .{method_idx});
                        return error.InvalidMethodIndex;
                    }
                    const closure_val = trait_inst.vtable.items[method_idx];
                    if (closure_val != .obj or closure_val.obj.type != .closure) {
                        std.debug.print("Method not implemented\n", .{});
                        return error.MethodNotImplemented;
                    }

                    const total_argc: u8 = arg_count + 1; // include self
                    const self_val = Value{ .obj = &trait_inst.instance.obj };

                    // Insert a slot for self after base_index
                    try self.stack.append(Value{ .nil = {} });
                    var j: usize = self.stack.items.len - 1;
                    while (j > base_index + 1) : (j -= 1) {
                        self.stack.items[j] = self.stack.items[j - 1];
                    }

                    // Place closure at base_index and self at base_index+1
                    self.stack.items[base_index] = closure_val;
                    self.stack.items[base_index + 1] = self_val;

                    // Now use the standard call path; callee is at base_index
                    if (!self.callValue(self.stack.items[base_index], total_argc)) {
                        return error.CallError;
                    }
                    frame = &self.frames.items[self.frames.items.len - 1];
                },
                .get_upvalue => {
                    const idx = self.readByte(frame);
                    if (frame.closure) |cl| {
                        if (idx >= cl.upvalues.items.len) {
                            std.debug.print("Invalid upvalue index {d}\n", .{idx});
                            return error.InvalidUpvalueIndex;
                        }
                        try self.push(cl.upvalues.items[idx]);
                    } else {
                        std.debug.print("No closure in current frame\n", .{});
                        return error.MissingClosure;
                    }
                },
                .set_upvalue => {
                    const idx = self.readByte(frame);
                    if (frame.closure) |cl| {
                        if (idx >= cl.upvalues.items.len) {
                            std.debug.print("Invalid upvalue index {d}\n", .{idx});
                            return error.InvalidUpvalueIndex;
                        }
                        cl.upvalues.items[idx] = self.peek(0);
                    } else {
                        std.debug.print("No closure in current frame\n", .{});
                        return error.MissingClosure;
                    }
                },
                .closure => {
                    const const_index = self.readByte(frame);
                    const val = frame.function.chunk.constants.items[const_index];
                    if (val != .obj or val.obj.type != .function) {
                        std.debug.print("OP_CLOSURE expects function constant\n", .{});
                        return error.BadClosureConstant;
                    }
                    const fn_ptr = @as(*ObjFunction, @fieldParentPtr("obj", val.obj));
                    var closure = try value.ObjClosure.init(self.allocator, fn_ptr);

                    var i: usize = 0;
                    while (i < fn_ptr.upvalue_count) : (i += 1) {
                        const is_local = self.readByte(frame);
                        const idx = self.readByte(frame);
                        if (is_local != 0) {
                            const slot_index = frame.slots + 1 + idx;
                            if (slot_index >= self.stack.items.len) {
                                std.debug.print("Invalid local slot {d}\n", .{slot_index});
                                return error.InvalidLocalSlot;
                            }
                            try closure.upvalues.append(self.stack.items[slot_index]);
                        } else {
                            if (frame.closure) |enc_cl| {
                                if (idx >= enc_cl.upvalues.items.len) {
                                    std.debug.print("Invalid enclosing upvalue index {d}\n", .{idx});
                                    return error.InvalidUpvalueIndex;
                                }
                                try closure.upvalues.append(enc_cl.upvalues.items[idx]);
                            } else {
                                // No enclosing closure: capture nil
                                try closure.upvalues.append(Value{ .nil = {} });
                            }
                        }
                    }

                    try self.push(Value{ .obj = &closure.obj });
                },
                .load_local => {
                    const slot = self.readByte(frame);
                    const idx = frame.slots + 1 + slot;
                    if (idx >= self.stack.items.len) {
                        std.debug.print("Invalid local slot {d} (slots={d}, slot={d}, stack_len={d})\n", .{ idx, frame.slots, slot, self.stack.items.len });
                        return error.InvalidLocalSlot;
                    }
                    if (trace.enabled) {
                        const v = self.stack.items[idx];
                        const tag = @as(std.meta.Tag(Value), v);
                        const ar = frame.function.arity;
                        if (v == .obj) {
                            trace.log("VM", "load_local idx={d} slots={d} slot={d} arity={d} stack_len={d} tag={s} obj={s}", .{ idx, frame.slots, slot, ar, self.stack.items.len, @tagName(tag), @tagName(v.obj.type) });
                        } else {
                            trace.log("VM", "load_local idx={d} slots={d} slot={d} arity={d} stack_len={d} tag={s}", .{ idx, frame.slots, slot, ar, self.stack.items.len, @tagName(tag) });
                        }
                        // dump a small window around the base and params to diagnose aliasing
                        const base = frame.slots;
                        var start: isize = @as(isize, @intCast(base)) - 2;
                        if (start < 0) start = 0;
                        var end: usize = base + 2 + @as(usize, ar);
                        if (end > self.stack.items.len) end = self.stack.items.len;
                        var i: usize = @intCast(start);
                        while (i < end) : (i += 1) {
                            const vv = self.stack.items[i];
                            const t = @as(std.meta.Tag(Value), vv);
                            if (vv == .obj) {
                                trace.log("VM", "  dump[i]={d} tag={s} obj={s}", .{ i, @tagName(t), @tagName(vv.obj.type) });
                            } else {
                                trace.log("VM", "  dump[i]={d} tag={s}", .{ i, @tagName(t) });
                            }
                        }
                    }
                    const val_local = self.stack.items[idx];
                    if (trace.enabled) {
                        const tag_vl = @as(std.meta.Tag(Value), val_local);
                        if (val_local == .obj) {
                            trace.log("VM", "load_local about to push tag={s} obj={s}", .{ @tagName(tag_vl), @tagName(val_local.obj.type) });
                        } else {
                            trace.log("VM", "load_local about to push tag={s}", .{@tagName(tag_vl)});
                        }
                    }
                    try self.push(val_local);
                },
                .store_local => {
                    const slot = self.readByte(frame);
                    const idx = frame.slots + 1 + slot;
                    // Pop the value first to avoid reducing length below idx during assignment
                    const val = self.pop();
                    if (trace.enabled) {
                        const tag = @as(std.meta.Tag(Value), val);
                        if (val == .obj) {
                            trace.log("VM", "store_local idx={d} slot={d} base={d} tag={s} obj={s} pre_len={d}", .{ idx, slot, frame.slots, @tagName(tag), @tagName(val.obj.type), self.stack.items.len });
                        } else {
                            trace.log("VM", "store_local idx={d} slot={d} base={d} tag={s} pre_len={d}", .{ idx, slot, frame.slots, @tagName(tag), self.stack.items.len });
                        }
                    }
                    // Ensure capacity and length for the target local slot
                    while (self.stack.items.len <= idx) {
                        try self.stack.append(Value{ .nil = {} });
                    }
                    self.stack.items[idx] = val;
                    if (trace.enabled) {
                        const v2 = self.stack.items[idx];
                        const tag2 = @as(std.meta.Tag(Value), v2);
                        if (v2 == .obj) {
                            trace.log("VM", "store_local set idx={d} tag={s} obj={s} post_len={d}", .{ idx, @tagName(tag2), @tagName(v2.obj.type), self.stack.items.len });
                        } else {
                            trace.log("VM", "store_local set idx={d} tag={s} post_len={d}", .{ idx, @tagName(tag2), self.stack.items.len });
                        }
                    }
                },
                .add => {
                    if (trace.enabled) {
                        const len = self.stack.items.len;
                        trace.log("VM", "ADD pre stack_len={d}", .{len});
                        var k: isize = @as(isize, @intCast(len)) - 1;
                        var count: usize = 0;
                        while (k >= 0 and count < 8) : (k -= 1) {
                            const idx: usize = @intCast(k);
                            const v = self.stack.items[idx];
                            const tag = @as(std.meta.Tag(Value), v);
                            if (v == .obj) {
                                trace.log("VM", "  idx={d} tag={s} obj={s}", .{ idx, @tagName(tag), @tagName(v.obj.type) });
                            } else {
                                trace.log("VM", "  idx={d} tag={s}", .{ idx, @tagName(tag) });
                            }
                            count += 1;
                        }
                    }
                    const b = self.pop();
                    const a = self.pop();

                    if (trace.enabled) {
                        const tagA = @as(std.meta.Tag(Value), a);
                        const tagB = @as(std.meta.Tag(Value), b);
                        if (a == .obj and b == .obj) {
                            trace.log("VM", "ADD popped a={s}/{s} b={s}/{s}", .{ @tagName(tagA), @tagName(a.obj.type), @tagName(tagB), @tagName(b.obj.type) });
                        } else if (a == .obj) {
                            trace.log("VM", "ADD popped a={s}/{s} b={s}", .{ @tagName(tagA), @tagName(a.obj.type), @tagName(tagB) });
                        } else if (b == .obj) {
                            trace.log("VM", "ADD popped a={s} b={s}/{s}", .{ @tagName(tagA), @tagName(tagB), @tagName(b.obj.type) });
                        } else {
                            trace.log("VM", "ADD popped a={s} b={s}", .{ @tagName(tagA), @tagName(tagB) });
                        }
                    }

                    if (a == .number and b == .number) {
                        try self.push(Value{ .number = a.number + b.number });
                    } else if (a == .obj and b == .obj and a.obj.type == .string and b.obj.type == .string) {
                        const a_str = @as(*value.ObjString, @fieldParentPtr("obj", a.obj)).*;
                        const b_str = @as(*value.ObjString, @fieldParentPtr("obj", b.obj)).*;

                        const combined = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ a_str.chars, b_str.chars });
                        defer self.allocator.free(combined);

                        const obj_str = try value.ObjString.init(self.allocator, combined);
                        try self.push(Value{ .obj = &obj_str.obj });
                    } else if (a == .obj and a.obj.type == .string and b == .number) {
                        const a_str = @as(*value.ObjString, @fieldParentPtr("obj", a.obj)).*;
                        const combined = try std.fmt.allocPrint(self.allocator, "{s}{d}", .{ a_str.chars, b.number });
                        defer self.allocator.free(combined);
                        const obj_str = try value.ObjString.init(self.allocator, combined);
                        try self.push(Value{ .obj = &obj_str.obj });
                    } else if (a == .number and b == .obj and b.obj.type == .string) {
                        const b_str = @as(*value.ObjString, @fieldParentPtr("obj", b.obj)).*;
                        const combined = try std.fmt.allocPrint(self.allocator, "{d}{s}", .{ a.number, b_str.chars });
                        defer self.allocator.free(combined);
                        const obj_str = try value.ObjString.init(self.allocator, combined);
                        try self.push(Value{ .obj = &obj_str.obj });
                    } else {
                        if (trace.enabled) {
                            const tagA = @as(std.meta.Tag(Value), a);
                            const tagB = @as(std.meta.Tag(Value), b);
                            std.debug.print("ADD type mismatch: a={s}", .{@tagName(tagA)});
                            if (a == .obj) std.debug.print(" a.obj={s}", .{@tagName(a.obj.type)});
                            std.debug.print(" b={s}", .{@tagName(tagB)});
                            if (b == .obj) std.debug.print(" b.obj={s}", .{@tagName(b.obj.type)});
                            std.debug.print("\n", .{});
                        }
                        std.debug.print("Operands must be two numbers or strings with a number\n", .{});
                        return error.OperandTypeMismatch;
                    }
                },
                .subtract => {
                    const b = self.pop();
                    const a = self.pop();
                    try self.binaryNumberOp(a, b, .subtract);
                },
                .multiply => {
                    const b = self.pop();
                    const a = self.pop();
                    try self.binaryNumberOp(a, b, .multiply);
                },
                .divide => {
                    const b = self.pop();
                    const a = self.pop();
                    try self.binaryNumberOp(a, b, .divide);
                },
                .negate => {
                    const value_i = self.pop();
                    if (value_i != .number) {
                        std.debug.print("Operand must be a number\n", .{});
                        return error.OperandTypeMismatch;
                    }
                    try self.push(Value{ .number = -value_i.number });
                },
                .equal => {
                    const b = self.pop();
                    const a = self.pop();
                    try self.push(Value{ .boolean = Value.equals(a, b) });
                },
                .greater => {
                    const b = self.pop();
                    const a = self.pop();
                    try self.binaryNumberOp(a, b, .greater);
                },
                .less => {
                    const b = self.pop();
                    const a = self.pop();
                    try self.binaryNumberOp(a, b, .less);
                },
                .jump => {
                    const offset = self.readShort(frame);
                    frame.ip += offset;
                },
                .jump_if_false => {
                    const offset = self.readShort(frame);
                    if (self.peek(0).isFalsey()) {
                        frame.ip += offset;
                    }
                },
                .loop => {
                    const offset = self.readShort(frame);
                    frame.ip -= offset;
                },
                .call => {
                    const arg_count = self.readByte(frame);
                    const cal = self.peek(arg_count);
                    if (trace.enabled and cal == .obj) {
                        switch (cal.obj.type) {
                            .closure => {
                                const cl = @as(*ObjClosure, @fieldParentPtr("obj", cal.obj));
                                trace.log("VM", "call opcode closure arity={d} argc={d}", .{ cl.function.arity, arg_count });
                            },
                            .function => {
                                const fnp = @as(*ObjFunction, @fieldParentPtr("obj", cal.obj));
                                trace.log("VM", "call opcode function arity={d} argc={d}", .{ fnp.arity, arg_count });
                            },
                            .native => {
                                trace.log("VM", "call opcode native argc={d}", .{arg_count});
                            },
                            else => {
                                trace.log("VM", "call opcode non-callable type", .{});
                            },
                        }
                    }
                    if (!self.callValue(cal, arg_count)) {
                        return error.CallError;
                    }
                    frame = &self.frames.items[self.frames.items.len - 1];
                },
                .@"return" => {
                    // Use the current frame's slots (base of callee) to place the result back
                    const result = self.pop();
                    const callee_slots = frame.slots;
                    _ = self.frames.pop();

                    if (self.frames.items.len == 0) {
                        // Returning from top-level script: done
                        return;
                    }

                    // Replace the callee position in the caller's stack with the result,
                    // then truncate the stack to exactly that single result slot.
                    self.stack.items[callee_slots] = result;
                    self.stack.shrinkRetainingCapacity(callee_slots + 1);

                    // Continue executing in the caller frame
                    frame = &self.frames.items[self.frames.items.len - 1];
                },
                .print => {
                    const value_i = self.pop();
                    value_i.print();
                    std.debug.print("\n", .{});
                },
                .pop => {
                    _ = self.pop();
                },
                .nil => {
                    try self.push(Value{ .nil = {} });
                },
                .true => {
                    try self.push(Value{ .boolean = true });
                },
                .false => {
                    try self.push(Value{ .boolean = false });
                },
                .not => {
                    try self.push(Value{ .boolean = self.pop().isFalsey() });
                },
            }
        }
    }

    fn binaryNumberOp(self: *VM, a: Value, b: Value, op: OpCode) !void {
        if (a != .number or b != .number) {
            std.debug.print("Operands must be numbers\n", .{});
            return error.OperandTypeMismatch;
        }

        switch (op) {
            .subtract => try self.push(Value{ .number = a.number - b.number }),
            .multiply => try self.push(Value{ .number = a.number * b.number }),
            .divide => try self.push(Value{ .number = a.number / b.number }),
            .greater => try self.push(Value{ .boolean = a.number > b.number }),
            .less => try self.push(Value{ .boolean = a.number < b.number }),
            else => unreachable,
        }
    }

    fn callValue(self: *VM, callee: Value, arg_count: u8) bool {
        if (true) {
            const obj = callee.obj;
            switch (obj.type) {
                .closure => {
                    const closure = @as(*ObjClosure, @fieldParentPtr("obj", obj));
                    const function = closure.function;
                    if (trace.enabled) trace.log("VM", "callValue closure arity={d} argc={d}", .{ function.arity, arg_count });
                    if (arg_count != function.*.arity) {
                        std.debug.print("Expected {d} arguments but got {d}\n", .{ function.arity, arg_count });
                        return false;
                    }
                    if (self.frames.items.len == FramesMax) {
                        std.debug.print("Stack overflow\n", .{});
                        return false;
                    }
                    if (trace.enabled) {
                        const base = self.stack.items.len - arg_count - 1;
                        var i: usize = if (base > 2) base - 2 else 0;
                        while (i < self.stack.items.len) : (i += 1) {
                            const v = self.stack.items[i];
                            const tag = @as(std.meta.Tag(Value), v);
                            if (v == .obj) {
                                trace.log("VM", "stack[i]={d} base={d} tag={s} obj={s}", .{ i, base, @tagName(tag), @tagName(v.obj.type) });
                            } else {
                                trace.log("VM", "stack[i]={d} base={d} tag={s}", .{ i, base, @tagName(tag) });
                            }
                        }
                        // Log call frame mapping expectations
                        trace.log("VM", "callValue.closure base={d} argc={d} local0_idx={d} local1_idx={d}", .{ base, arg_count, base + 1, base + 2 });
                        // Log concrete values at arg positions
                        if (arg_count > 0) {
                            const v0 = self.stack.items[base + 1];
                            const tag0 = @as(std.meta.Tag(Value), v0);
                            if (v0 == .obj) trace.log("VM", "arg0 tag={s} obj={s}", .{ @tagName(tag0), @tagName(v0.obj.type) }) else trace.log("VM", "arg0 tag={s}", .{@tagName(tag0)});
                        }
                        if (arg_count > 1) {
                            const v1 = self.stack.items[base + 2];
                            const tag1 = @as(std.meta.Tag(Value), v1);
                            if (v1 == .obj) trace.log("VM", "arg1 tag={s} obj={s}", .{ @tagName(tag1), @tagName(v1.obj.type) }) else trace.log("VM", "arg1 tag={s}", .{@tagName(tag1)});
                        }
                    }
                    self.frames.append(.{
                        .function = function,
                        .closure = closure,
                        .ip = 0,
                        .slots = self.stack.items.len - arg_count - 1,
                    }) catch return false;
                    return true;
                },
                .function => {
                    const function = @as(*ObjFunction, @fieldParentPtr("obj", obj));
                    if (trace.enabled) trace.log("VM", "callValue function arity={d} argc={d}", .{ function.arity, arg_count });
                    if (arg_count != function.*.arity) {
                        std.debug.print("Expected {d} arguments but got {d}\n", .{ function.arity, arg_count });
                        return false;
                    }

                    if (self.frames.items.len == FramesMax) {
                        std.debug.print("Stack overflow\n", .{});
                        return false;
                    }

                    if (trace.enabled) {
                        const base = self.stack.items.len - arg_count - 1;
                        var i: usize = if (base > 2) base - 2 else 0;
                        while (i < self.stack.items.len) : (i += 1) {
                            const v = self.stack.items[i];
                            const tag = @as(std.meta.Tag(Value), v);
                            if (v == .obj) {
                                trace.log("VM", "stack[i]={d} base={d} tag={s} obj={s}", .{ i, base, @tagName(tag), @tagName(v.obj.type) });
                            } else {
                                trace.log("VM", "stack[i]={d} base={d} tag={s}", .{ i, base, @tagName(tag) });
                            }
                        }
                    }

                    self.frames.append(.{
                        .function = function,
                        .closure = null,
                        .ip = 0,
                        .slots = self.stack.items.len - arg_count - 1,
                    }) catch return false;
                    return true;
                },
                .native => {
                    const native = @as(*ObjNative, @fieldParentPtr("obj", obj)).*;
                    const args = self.stack.items[self.stack.items.len - arg_count ..];
                    const result = native.function(arg_count, args);

                    self.stack.shrinkRetainingCapacity(self.stack.items.len - arg_count - 1);
                    self.push(result) catch return false;
                    return true;
                },
                else => {},
            }
        }

        std.debug.print("Can only call functions and classes\n", .{});
        return false;
    }

    fn readByte(_: *VM, frame: *CallFrame) u8 {
        const byte = frame.function.chunk.code.items[frame.ip];
        frame.ip += 1;
        return byte;
    }

    fn readShort(_: *VM, frame: *CallFrame) u16 {
        frame.ip += 2;
        return @as(u16, frame.function.chunk.code.items[frame.ip - 2]) |
            (@as(u16, frame.function.chunk.code.items[frame.ip - 1]) << 8);
    }

    fn readConstant(self: *VM, frame: *CallFrame) Value {
        const constant = self.readByte(frame);
        return frame.function.chunk.constants.items[constant];
    }

    fn push(self: *VM, value_o: Value) !void {
        if (self.stack.items.len >= StackMax) {
            std.debug.print("Stack overflow\n", .{});
            return error.StackOverflow;
        }
        try self.stack.append(value_o);
        if (trace.enabled) {
            const idx: usize = self.stack.items.len - 1;
            const tag = @as(std.meta.Tag(Value), value_o);
            if (value_o == .obj) {
                trace.log("VM", "push idx={d} tag={s} obj={s} new_len={d}", .{ idx, @tagName(tag), @tagName(value_o.obj.type), self.stack.items.len });
            } else {
                trace.log("VM", "push idx={d} tag={s} new_len={d}", .{ idx, @tagName(tag), self.stack.items.len });
            }
        }
    }

    fn pop(self: *VM) Value {
        const v = self.stack.pop().?;
        if (trace.enabled) {
            const tag = @as(std.meta.Tag(Value), v);
            if (v == .obj) {
                trace.log("VM", "pop tag={s} obj={s} new_len={d}", .{ @tagName(tag), @tagName(v.obj.type), self.stack.items.len });
            } else {
                trace.log("VM", "pop tag={s} new_len={d}", .{ @tagName(tag), self.stack.items.len });
            }
        }
        return v;
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - distance];
    }
};
