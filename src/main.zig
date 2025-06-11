const std = @import("std");
const lib = @import("TofuScript_lib");

const Opcode = enum(u8) {
    push = 0x01,
    pop = 0x02,
    add = 0x03,
    sub = 0x04,
    mul = 0x05,
    div = 0x06,
    halt = 0x00,
};

const VmError = error{
    StackUnderflow,
    StackOverflow,
    DivisionByZero,
    InvalidOpcode,
};

const VM = struct {
    stack: [256]i32 = undefined,
    sp: usize = 0, // Stack pointer
    pc: usize = 0, // Program counter
    program: []const u8,
    debug: bool = false,

    /// Print current stack contents
    fn printStack(self: *const VM) void {
        std.debug.print("Stack [sp={d}]: ", .{self.sp});
        for (0..self.sp) |i| {
            if (i > 0) std.debug.print(", ", .{});
            std.debug.print("{}", .{self.stack[i]});
        }
        std.debug.print("\n", .{});
    }

    /// Print instruction being executed
    fn debugPrintInstruction(self: *const VM, op: Opcode) void {
        const opname = switch (op) {
            .push => "push",
            .pop => "pop",
            .add => "add",
            .sub => "sub",
            .mul => "mul",
            .div => "div",
            .halt => "halt",
        };

        std.debug.print("PC: {d:0>3} - {s}", .{ self.pc - 1, opname });

        // Handle special cases
        switch (op) {
            .push => {
                if (self.pc - 1 < self.program.len) {
                    std.debug.print(" {d}", .{self.program[self.pc]});
                } else {
                    std.debug.print(" <missing operand>", .{});
                }
            },
            else => {},
        }
        std.debug.print("\n", .{});
    }

    fn run(self: *VM) VmError!void {
        while (self.pc < self.program.len) {
            const op_byte = self.program[self.pc];
            self.pc += 1;

            const op = std.meta.intToEnum(Opcode, op_byte) catch {
                std.debug.print("Invalid opcode: 0x{X}\n", .{op_byte});
                return VmError.InvalidOpcode;
            };

            // Debug output
            if (self.debug) {
                self.debugPrintInstruction(op);
            }

            switch (op) {
                .push => {
                    if (self.sp >= self.stack.len) return VmError.StackOverflow;
                    const value = self.program[self.pc];
                    self.pc += 1;
                    self.stack[self.sp] = @as(i32, value);
                    self.sp += 1;
                },
                .pop => {
                    if (self.sp == 0) return VmError.StackUnderflow;
                    self.sp -= 1;
                },
                .add, .sub, .mul, .div => {
                    if (self.sp < 2) return VmError.StackUnderflow;
                    const b = self.stack[self.sp - 1];
                    const a = self.stack[self.sp - 2];
                    self.sp -= 1;

                    switch (op) {
                        .add => self.stack[self.sp - 1] = a + b,
                        .sub => self.stack[self.sp - 1] = a - b,
                        .mul => self.stack[self.sp - 1] = a * b,
                        .div => {
                            if (b == 0) return VmError.DivisionByZero;
                            self.stack[self.sp - 1] = @divTrunc(a, b);
                        },
                        else => unreachable,
                    }
                },
                .halt => return,
            }

            // Show stack after instruction execution
            if (self.debug) {
                self.printStack();
                std.debug.print("----\n", .{});
            }
        }
    }
};

pub fn main() !void {
    // Example program: (5 * 3) + (10 / 2) = 15 + 5 = 20
    const program = [_]u8{
        @intFromEnum(Opcode.push), 5,
        @intFromEnum(Opcode.push), 3,
        @intFromEnum(Opcode.mul),  @intFromEnum(Opcode.push),
        10,                        @intFromEnum(Opcode.push),
        2,                         @intFromEnum(Opcode.div),
        @intFromEnum(Opcode.add),  @intFromEnum(Opcode.halt),
    };

    var vm = VM{
        .program = &program,
        .debug = true, // Enable debug output
    };

    try vm.run();

    // Print final result
    std.debug.print("\nFinal result: ", .{});
    if (vm.sp > 0) {
        std.debug.print("{}\n", .{vm.stack[vm.sp - 1]});
    } else {
        std.debug.print("<empty stack>\n", .{});
    }
}
