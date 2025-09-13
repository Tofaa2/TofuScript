const std = @import("std");
const v = @import("value.zig");
const Value = v.Value;
const ObjFunction = v.ObjFunction;
const trace = @import("trace.zig");

pub const OpCode = enum(u8) {
    // Constants
    constant,
    constant_long,

    // Variables
    get_global,
    set_global,
    define_global,
    // Locals
    load_local,
    store_local,
    // Upvalues / Closures
    get_upvalue,
    set_upvalue,
    closure,

    // Structs
    new_instance,
    get_field,
    set_field,

    // Traits
    cast_to_trait,
    invoke_trait,

    // Operations
    add,
    subtract,
    multiply,
    divide,
    negate,

    // Comparison
    equal,
    greater,
    less,

    // Control flow
    jump,
    jump_if_false,
    loop,
    call,
    @"return",

    // Special
    print,
    pop,
    nil,
    true,
    false,
    not,
    dup,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    constants: std.ArrayList(Value),
    lines: std.ArrayList(usize), // For debugging

    pub fn init(allocator: std.mem.Allocator) Chunk {
        return Chunk{
            .code = std.ArrayList(u8).init(allocator),
            .constants = std.ArrayList(Value).init(allocator),
            .lines = std.ArrayList(usize).init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn write(self: *Chunk, byte: u8, line: usize) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn addConstant(self: *Chunk, value: Value) !u24 {
        try self.constants.append(value);
        const index = self.constants.items.len - 1;
        trace.log("BC", "add const idx={d} total={d}", .{ index, self.constants.items.len });
        return @intCast(index);
    }

    pub fn disassemble(self: *Chunk, name: []const u8) void {
        std.debug.print("== {s} ==\n", .{name});

        var offset: usize = 0;
        while (offset < self.code.items.len) {
            offset = self.disassembleInstruction(offset);
        }
    }

    pub fn disassembleInstruction(self: *Chunk, offset: usize) usize {
        std.debug.print("{d:0>4} ", .{offset});

        if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
            std.debug.print("   | ", .{});
        } else {
            std.debug.print("{d:4} ", .{self.lines.items[offset]});
        }

        const instruction = @as(OpCode, @enumFromInt(self.code.items[offset]));
        switch (instruction) {
            .constant => return self.constantInstruction("OP_CONSTANT", offset),
            .constant_long => return self.constantLongInstruction("OP_CONSTANT_LONG", offset),
            .get_global => return self.constantInstruction("OP_GET_GLOBAL", offset),
            .set_global => return self.constantInstruction("OP_SET_GLOBAL", offset),
            .define_global => return self.constantInstruction("OP_DEFINE_GLOBAL", offset),
            .load_local => return self.byteInstruction("OP_LOAD_LOCAL", offset),
            .store_local => return self.byteInstruction("OP_STORE_LOCAL", offset),
            .get_upvalue => return self.byteInstruction("OP_GET_UPVALUE", offset),
            .set_upvalue => return self.byteInstruction("OP_SET_UPVALUE", offset),
            .closure => return self.closureInstruction("OP_CLOSURE", offset),
            .new_instance => return self.simpleInstruction("OP_NEW_INSTANCE", offset),
            .get_field => return self.constantInstruction("OP_GET_FIELD", offset),
            .set_field => return self.constantInstruction("OP_SET_FIELD", offset),
            .cast_to_trait => return self.simpleInstruction("OP_CAST_TO_TRAIT", offset),
            .invoke_trait => return self.twoByteInstruction("OP_INVOKE_TRAIT", offset),
            .add => return self.simpleInstruction("OP_ADD", offset),
            .subtract => return self.simpleInstruction("OP_SUBTRACT", offset),
            .multiply => return self.simpleInstruction("OP_MULTIPLY", offset),
            .divide => return self.simpleInstruction("OP_DIVIDE", offset),
            .negate => return self.simpleInstruction("OP_NEGATE", offset),
            .equal => return self.simpleInstruction("OP_EQUAL", offset),
            .greater => return self.simpleInstruction("OP_GREATER", offset),
            .less => return self.simpleInstruction("OP_LESS", offset),
            .jump => return self.jumpInstruction("OP_JUMP", 1, offset),
            .jump_if_false => return self.jumpInstruction("OP_JUMP_IF_FALSE", 1, offset),
            .loop => return self.jumpInstruction("OP_LOOP", -1, offset),
            .call => return self.byteInstruction("OP_CALL", offset),
            .@"return" => return self.simpleInstruction("OP_RETURN", offset),

            .print => return self.simpleInstruction("OP_PRINT", offset),
            .pop => return self.simpleInstruction("OP_POP", offset),
            .nil => return self.simpleInstruction("OP_NIL", offset),
            .true => return self.simpleInstruction("OP_TRUE", offset),
            .false => return self.simpleInstruction("OP_FALSE", offset),
            .not => return self.simpleInstruction("OP_NOT", offset),
            .dup => return self.simpleInstruction("OP_DUP", offset),
        }
    }

    fn simpleInstruction(_: *Chunk, name: []const u8, offset: usize) usize {
        std.debug.print("{s}\n", .{name});
        return offset + 1;
    }

    fn byteInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        const slot = self.code.items[offset + 1];
        std.debug.print("{s} {d}\n", .{ name, slot });
        return offset + 2;
    }

    fn twoByteInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        const a = self.code.items[offset + 1];
        const b = self.code.items[offset + 2];
        std.debug.print("{s} {d} {d}\n", .{ name, a, b });
        return offset + 3;
    }

    fn constantInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        const constant = self.code.items[offset + 1];
        std.debug.print("{s} {d} '", .{ name, constant });
        self.constants.items[constant].print();
        std.debug.print("'\n", .{});
        return offset + 2;
    }

    fn constantLongInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        const constant = @as(u24, self.code.items[offset + 1]) |
            (@as(u24, self.code.items[offset + 2]) << 8) |
            (@as(u24, self.code.items[offset + 3]) << 16);
        std.debug.print("{s} {d} '", .{ name, constant });
        self.constants.items[constant].print();
        std.debug.print("'\n", .{});
        return offset + 4;
    }

    fn closureInstruction(self: *Chunk, name: []const u8, offset: usize) usize {
        const constant = self.code.items[offset + 1];
        std.debug.print("{s} {d} '", .{ name, constant });
        const val = self.constants.items[constant];
        // Expect a function object at this constant
        if (val == .obj) {
            const obj = val.obj;
            // Attempt to treat as function for disassembly purposes
            // If not function, print generically
            if (obj.type == .function) {
                const func = @as(*ObjFunction, @fieldParentPtr("obj", obj)).*;
                if (func.name) |nm| {
                    std.debug.print("<fn {s}>", .{nm.chars});
                } else {
                    std.debug.print("<script>", .{});
                }
                std.debug.print("'\n", .{});
                // Print captured upvalue mappings
                var off: usize = offset + 2;
                var i: usize = 0;
                while (i < func.upvalue_count) : (i += 1) {
                    const is_local = self.code.items[off];
                    const idx = self.code.items[off + 1];
                    std.debug.print("      | upvalue {d}: {s} {d}\n", .{ i, if (is_local != 0) "local" else "upvalue", idx });
                    off += 2;
                }
                return off;
            }
        }
        std.debug.print("'\n", .{});
        return offset + 2;
    }

    fn jumpInstruction(self: *Chunk, name: []const u8, sign: i8, offset: usize) usize {
        const jump = @as(u16, self.code.items[offset + 1]) |
            (@as(u16, self.code.items[offset + 2]) << 8);

        const from_pos: i32 = @intCast(offset);
        const delta: i32 = (@as(i32, sign)) * @as(i32, @intCast(jump));
        const to_pos: i32 = from_pos + 3 + delta;

        std.debug.print("{s} {d} -> {d}\n", .{ name, from_pos, to_pos });
        return offset + 3;
    }
};
