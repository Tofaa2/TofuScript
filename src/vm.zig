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
                    if (self.globals.get(name.chars)) |value_i| {
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
                        std.debug.print("Invalid local slot {d}\n", .{idx});
                        return error.InvalidLocalSlot;
                    }
                    try self.push(self.stack.items[idx]);
                },
                .store_local => {
                    const slot = self.readByte(frame);
                    const idx = frame.slots + 1 + slot;
                    if (idx >= self.stack.items.len) {
                        std.debug.print("Invalid local slot {d}\n", .{idx});
                        return error.InvalidLocalSlot;
                    }
                    self.stack.items[idx] = self.peek(0);
                },
                .add => {
                    const b = self.pop();
                    const a = self.pop();

                    if (a == .number and b == .number) {
                        try self.push(Value{ .number = a.number + b.number });
                    } else if (a.obj.type == .string and b.obj.type == .string) {
                        const a_str = @as(*value.ObjString, @fieldParentPtr("obj", a.obj)).*;
                        const b_str = @as(*value.ObjString, @fieldParentPtr("obj", b.obj)).*;

                        const combined = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ a_str.chars, b_str.chars });
                        defer self.allocator.free(combined);

                        const obj_str = try value.ObjString.init(self.allocator, combined);
                        try self.push(Value{ .obj = &obj_str.obj });
                    } else {
                        std.debug.print("Operands must be two numbers or two strings\n", .{});
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
                    if (!self.callValue(self.peek(arg_count), arg_count)) {
                        return error.CallError;
                    }
                    frame = &self.frames.items[self.frames.items.len - 1];
                },
                .@"return" => {
                    const result = self.pop();
                    _ = self.frames.pop();

                    if (self.frames.items.len == 0) {
                        return;
                    }

                    self.stack.items[self.frames.items[self.frames.items.len - 1].slots] = result;
                    self.stack.shrinkRetainingCapacity(self.frames.items[self.frames.items.len - 1].slots + 1);
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
                    if (arg_count != function.*.arity) {
                        std.debug.print("Expected {d} arguments but got {d}\n", .{ function.arity, arg_count });
                        return false;
                    }
                    if (self.frames.items.len == FramesMax) {
                        std.debug.print("Stack overflow\n", .{});
                        return false;
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
                    if (arg_count != function.*.arity) {
                        std.debug.print("Expected {d} arguments but got {d}\n", .{ function.arity, arg_count });
                        return false;
                    }

                    if (self.frames.items.len == FramesMax) {
                        std.debug.print("Stack overflow\n", .{});
                        return false;
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
    }

    fn pop(self: *VM) Value {
        return self.stack.pop().?;
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - distance];
    }
};
