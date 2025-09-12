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
    struct_type,
    instance,
    trait_type,
    trait_instance,
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
            .struct_type => {
                const ty = @as(*ObjStructType, @fieldParentPtr("obj", self)).*;
                std.debug.print("<struct {s}>", .{ty.name.chars});
            },
            .instance => {
                const inst = @as(*ObjInstance, @fieldParentPtr("obj", self)).*;
                std.debug.print("<instance {s}>", .{inst.ty.name.chars});
            },
            .trait_type => {
                const ty = @as(*ObjTraitType, @fieldParentPtr("obj", self)).*;
                std.debug.print("<trait {s}>", .{ty.name.chars});
            },
            .trait_instance => {
                const inst = @as(*ObjTraitInstance, @fieldParentPtr("obj", self)).*;
                std.debug.print("<trait_instance {s}>", .{inst.trait_ty.name.chars});
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
                _ = b_function;
                _ = a_function;
                return false;
            },
            .closure => {
                const a_closure = @as(*ObjClosure, @fieldParentPtr("obj", a));
                const b_closure = @as(*ObjClosure, @fieldParentPtr("obj", b));
                return a_closure == b_closure;
            },
            .struct_type => {
                const a_ty = @as(*ObjStructType, @fieldParentPtr("obj", a));
                const b_ty = @as(*ObjStructType, @fieldParentPtr("obj", b));
                return a_ty == b_ty;
            },
            .instance => {
                const a_inst = @as(*ObjInstance, @fieldParentPtr("obj", a));
                const b_inst = @as(*ObjInstance, @fieldParentPtr("obj", b));
                return a_inst == b_inst;
            },
            .trait_type => {
                const a_ty = @as(*ObjTraitType, @fieldParentPtr("obj", a));
                const b_ty = @as(*ObjTraitType, @fieldParentPtr("obj", b));
                return a_ty == b_ty;
            },
            .trait_instance => {
                const a_inst = @as(*ObjTraitInstance, @fieldParentPtr("obj", a));
                const b_inst = @as(*ObjTraitInstance, @fieldParentPtr("obj", b));
                return a_inst == b_inst;
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

pub const ObjStructType = struct {
    obj: Obj = .{ .type = .struct_type },
    name: *ObjString,
    field_names: std.ArrayList([]const u8),
    field_index: std.StringHashMap(usize),

    pub fn init(allocator: std.mem.Allocator, name_chars: []const u8, fields: []const []const u8) !*ObjStructType {
        const ty = try allocator.create(ObjStructType);
        ty.* = .{
            .name = try ObjString.init(allocator, name_chars),
            .field_names = std.ArrayList([]const u8).init(allocator),
            .field_index = std.StringHashMap(usize).init(allocator),
        };
        // Copy field names and build index
        for (fields) |f| {
            const dup = try allocator.dupe(u8, f);
            try ty.field_names.append(dup);
        }
        var i: usize = 0;
        while (i < ty.field_names.items.len) : (i += 1) {
            try ty.field_index.put(ty.field_names.items[i], i);
        }
        return ty;
    }

    pub fn deinit(self: *ObjStructType, allocator: std.mem.Allocator) void {
        for (self.field_names.items) |fname| {
            allocator.free(fname);
        }
        self.field_names.deinit();
        self.field_index.deinit();
        self.name.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn fieldCount(self: *ObjStructType) usize {
        return self.field_names.items.len;
    }

    pub fn indexOf(self: *ObjStructType, name: []const u8) ?usize {
        return self.field_index.get(name);
    }
};

pub const ObjInstance = struct {
    obj: Obj = .{ .type = .instance },
    ty: *ObjStructType,
    fields: std.ArrayList(Value),

    pub fn init(allocator: std.mem.Allocator, ty: *ObjStructType) !*ObjInstance {
        const inst = try allocator.create(ObjInstance);
        inst.* = .{
            .ty = ty,
            .fields = std.ArrayList(Value).init(allocator),
        };
        // initialize with nils
        try inst.fields.ensureTotalCapacity(ty.fieldCount());
        var i: usize = 0;
        while (i < ty.fieldCount()) : (i += 1) {
            inst.fields.appendAssumeCapacity(Value{ .nil = {} });
        }
        return inst;
    }

    pub fn deinit(self: *ObjInstance, allocator: std.mem.Allocator) void {
        self.fields.deinit();
        allocator.destroy(self);
    }
};

pub const ObjTraitType = struct {
    obj: Obj = .{ .type = .trait_type },
    name: *ObjString,
    method_names: std.ArrayList([]const u8),

    pub fn init(allocator: std.mem.Allocator, name_chars: []const u8, methods: []const []const u8) !*ObjTraitType {
        const ty = try allocator.create(ObjTraitType);
        ty.* = .{
            .name = try ObjString.init(allocator, name_chars),
            .method_names = std.ArrayList([]const u8).init(allocator),
        };
        for (methods) |m| {
            const dup = try allocator.dupe(u8, m);
            try ty.method_names.append(dup);
        }
        return ty;
    }

    pub fn deinit(self: *ObjTraitType, allocator: std.mem.Allocator) void {
        for (self.method_names.items) |mname| {
            allocator.free(mname);
        }
        self.method_names.deinit();
        self.name.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn methodIndex(self: *ObjTraitType, name: []const u8) ?usize {
        for (self.method_names.items, 0..) |m, i| {
            if (std.mem.eql(u8, m, name)) return i;
        }
        return null;
    }
};

pub const ObjTraitInstance = struct {
    obj: Obj = .{ .type = .trait_instance },
    trait_ty: *ObjTraitType,
    instance: *ObjInstance,
    vtable: std.ArrayList(Value), // closures

    pub fn init(allocator: std.mem.Allocator, trait_ty: *ObjTraitType, instance: *ObjInstance) !*ObjTraitInstance {
        const inst = try allocator.create(ObjTraitInstance);
        inst.* = .{
            .trait_ty = trait_ty,
            .instance = instance,
            .vtable = std.ArrayList(Value).init(allocator),
        };
        // initialize vtable with nils
        try inst.vtable.ensureTotalCapacity(trait_ty.method_names.items.len);
        var i: usize = 0;
        while (i < trait_ty.method_names.items.len) : (i += 1) {
            inst.vtable.appendAssumeCapacity(Value{ .nil = {} });
        }
        return inst;
    }

    pub fn deinit(self: *ObjTraitInstance, allocator: std.mem.Allocator) void {
        self.vtable.deinit();
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
