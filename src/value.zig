const std = @import("std");
const Chunk = @import("bytecode.zig").Chunk;

pub const Value = union(enum) {
    number: f64,
    boolean: bool,
    obj: *Obj,
    nil,

    pub fn print(self: Value) void {
        switch (self) {
            .number => |n| std.debug.print("{d}", .{n}),
            .boolean => |b| std.debug.print("{}", .{b}),
            .obj => |obj| obj.print(),
            .nil => std.debug.print("nil", .{}),
        }
    }

    pub fn isFalsey(self: Value) bool {
        return switch (self) {
            .nil => true,
            .boolean => |b| !b,
            else => false,
        };
    }

    pub fn equals(a: Value, b: Value) bool {
        if (@as(std.meta.Tag(Value), a) != @as(std.meta.Tag(Value), b)) {
            return false;
        }

        return switch (a) {
            .number => |a_num| a_num == b.number,
            .boolean => |a_bool| a_bool == b.boolean,
            .nil => true,
            .obj => |a_obj| a_obj.equals(b.obj),
        };
    }
};

pub const ObjType = enum {
    string,
    function,
    closure,
    native,
};

pub const Obj = struct {
    type: ObjType,
    next: ?*Obj = null,

    pub fn print(self: *Obj) void {
        switch (self.type) {
            .string => {
                const string = @as(*ObjString, @fieldParentPtr("obj", self)).*;
                std.debug.print("{s}", .{string.chars});
            },
            .function => {
                const function = @as(*ObjFunction, @fieldParentPtr("obj", self)).*;
                if (function.name) |name| {
                    std.debug.print("<fn {s}>", .{name.chars});
                } else {
                    std.debug.print("<script>", .{});
                }
            },
            .closure => {
                const closure = @as(*ObjClosure, @fieldParentPtr("obj", self)).*;
                if (closure.function.name) |name| {
                    std.debug.print("<closure {s}>", .{name.chars});
                } else {
                    std.debug.print("<closure>", .{});
                }
            },
            .native => std.debug.print("<native fn>", .{}),
        }
    }

    pub fn equals(a: *Obj, b: *Obj) bool {
        if (a.type != b.type) {
            return false;
        }

        return switch (a.type) {
            .string => {
                const a_string = @as(*ObjString, @fieldParentPtr("obj", a)).*;
                const b_string = @as(*ObjString, @fieldParentPtr("obj", b)).*;
                return std.mem.eql(u8, a_string.chars, b_string.chars);
            },
            .function => {
                const a_function = @as(*ObjFunction, @fieldParentPtr("obj", a)).*;
                const b_function = @as(*ObjFunction, @fieldParentPtr("obj", b)).*;
                // Equality for functions is not supported yet.
                _ = b_function;
                _ = a_function;
                return false;
            },
            .closure => {
                const a_closure = @as(*ObjClosure, @fieldParentPtr("obj", a));
                const b_closure = @as(*ObjClosure, @fieldParentPtr("obj", b));
                return a_closure == b_closure;
            },
            .native => {
                const a_native = @as(*ObjNative, @fieldParentPtr("obj", a)).*;
                const b_native = @as(*ObjNative, @fieldParentPtr("obj", b)).*;
                return a_native.function == b_native.function;
            },
        };
    }
};

pub const ObjString = struct {
    obj: Obj = .{ .type = .string },
    chars: []const u8,

    pub fn init(allocator: std.mem.Allocator, chars: []const u8) !*ObjString {
        const string = try allocator.create(ObjString);
        string.* = .{
            .chars = try allocator.dupe(u8, chars),
        };
        return string;
    }

    pub fn deinit(self: *ObjString, allocator: std.mem.Allocator) void {
        allocator.free(self.chars);
        allocator.destroy(self);
    }
};

pub const ObjFunction = struct {
    obj: Obj = .{ .type = .function },
    arity: u8,
    upvalue_count: u8 = 0,
    chunk: Chunk,
    name: ?*ObjString = null,

    pub fn init(allocator: std.mem.Allocator) !*ObjFunction {
        const function = try allocator.create(ObjFunction);
        function.* = .{
            .arity = 0,
            .upvalue_count = 0,
            .chunk = Chunk.init(allocator),
        };
        return function;
    }

    pub fn deinit(self: *ObjFunction, allocator: std.mem.Allocator) void {
        if (self.name) |name| {
            name.deinit(allocator);
        }
        self.chunk.deinit();
        allocator.destroy(self);
    }
};

pub const ObjClosure = struct {
    obj: Obj = .{ .type = .closure },
    function: *ObjFunction,
    upvalues: std.ArrayList(Value),

    pub fn init(allocator: std.mem.Allocator, function: *ObjFunction) !*ObjClosure {
        const closure = try allocator.create(ObjClosure);
        closure.* = .{
            .function = function,
            .upvalues = std.ArrayList(Value).init(allocator),
        };
        return closure;
    }

    pub fn deinit(self: *ObjClosure, allocator: std.mem.Allocator) void {
        self.upvalues.deinit();
        allocator.destroy(self);
    }
};

pub const NativeFn = *const fn (arg_count: u8, args: []Value) Value;

pub const ObjNative = struct {
    obj: Obj = .{ .type = .native },
    function: NativeFn,

    pub fn init(allocator: std.mem.Allocator, function: NativeFn) !*ObjNative {
        const native = try allocator.create(ObjNative);
        native.* = .{
            .function = function,
        };
        return native;
    }

    pub fn deinit(self: *ObjNative, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }
};
