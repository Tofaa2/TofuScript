const std = @import("std");
const Value = @import("value.zig").Value;
const trace = @import("trace.zig");

pub const OpCode = enum(u8) {
    // Constants
    constant,
    constant_long,

    // Variables
    get_global,
    set_global,
    define_global,

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
