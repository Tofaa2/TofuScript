const std = @import("std");

/// Phase 2.5 foundational type system
/// - Nominal core types: Number, String, Bool, Nil
/// - User nominal types by name (struct, trait) are represented as .user with their name
/// - Function types have parameter and return types (no generics yet)
/// - Unknown used during inference or when annotations missing (depending on mode)
pub const Builtin = enum {
    Number,
    String,
    Bool,
    Nil,
};

pub const Type = union(enum) {
    builtin: Builtin,
    user: []const u8, // nominal user type name (struct or trait by name)
    function: *const FunctionSig,
    unknown,

    pub fn format(self: Type, writer: anytype) !void {
        switch (self) {
            .builtin => |b| try writer.print("{s}", .{builtinName(b)}),
            .user => |n| try writer.print("{s}", .{n}),
            .function => |f| {
                const fs = f.*;
                try writer.print("func(", .{});
                var i: usize = 0;
                while (i < fs.param_types.len) : (i += 1) {
                    try fs.param_types[i].*.format(writer);
                    if (i + 1 < fs.param_types.len) try writer.print(", ", .{});
                }
                try writer.print("): ", .{});
                try fs.return_type.*.format(writer);
            },
            .unknown => try writer.print("Unknown", .{}),
        }
    }

    pub fn equals(a: Type, b: Type) bool {
        if (@as(std.meta.Tag(Type), a) != @as(std.meta.Tag(Type), b)) return false;
        return switch (a) {
            .builtin => |ab| ab == b.builtin,
            .user => |an| std.mem.eql(u8, an, b.user),
            .function => |af| fnEqualsPtr(af, b.function),
            .unknown => true, // Unknown equals Unknown
        };
    }

    // Assignability: strict equality for now; Nil assignable to user (nullable) could be added later.
    pub fn isAssignable(to_ty: Type, from_ty: Type) bool {
        // Unknown can flow to anything in loose mode; here we keep strict policy (caller can relax).
        if (isUnknown(to_ty) or isUnknown(from_ty)) return false;

        // Exact match or simple coercions
        if (equals(to_ty, from_ty)) return true;

        // Future: allow Nil to any reference-like type; currently disabled
        // if (from_ty == .builtin and from_ty.builtin == .Nil) return true;

        return false;
    }

    pub fn isUnknown(t: Type) bool {
        return @as(std.meta.Tag(Type), t) == .unknown;
    }
};

pub const FunctionSig = struct {
    param_types: []const *const Type,
    return_type: *const Type,
};

pub const Mode = enum { strict, loose };

pub const Env = struct {
    allocator: std.mem.Allocator,

    // Nominal registries
    struct_types: std.StringHashMap(void), // set: struct names
    trait_types: std.StringHashMap(void), // set: trait names

    pub fn init(allocator: std.mem.Allocator) Env {
        return .{
            .allocator = allocator,
            .struct_types = std.StringHashMap(void).init(allocator),
            .trait_types = std.StringHashMap(void).init(allocator),
        };
    }

    pub fn deinit(self: *Env) void {
        self.struct_types.deinit();
        self.trait_types.deinit();
    }

    pub fn registerStruct(self: *Env, name: []const u8) !void {
        try self.struct_types.put(try self.allocator.dupe(u8, name), {});
    }

    pub fn registerTrait(self: *Env, name: []const u8) !void {
        try self.trait_types.put(try self.allocator.dupe(u8, name), {});
    }

    pub fn isStruct(self: *Env, name: []const u8) bool {
        return self.struct_types.contains(name);
    }

    pub fn isTrait(self: *Env, name: []const u8) bool {
        return self.trait_types.contains(name);
    }
};

pub fn builtinByName(name: []const u8) ?Builtin {
    if (std.mem.eql(u8, name, "Number")) return .Number;
    if (std.mem.eql(u8, name, "String")) return .String;
    if (std.mem.eql(u8, name, "Bool")) return .Bool;
    if (std.mem.eql(u8, name, "Nil")) return .Nil;
    return null;
}

pub fn builtinName(b: Builtin) []const u8 {
    return switch (b) {
        .Number => "Number",
        .String => "String",
        .Bool => "Bool",
        .Nil => "Nil",
    };
}

/// Try to interpret a nominal identifier into a Type:
/// - Builtins are recognized by name
/// - Otherwise return a .user(name) nominal; caller can later validate via Env
pub fn nominalFromIdent(name: []const u8) Type {
    if (builtinByName(name)) |b| return Type{ .builtin = b };
    return Type{ .user = name };
}

/// Helper to print a type to a temporary buffer (for error messages)
pub fn typeToString(allocator: std.mem.Allocator, t: Type) ![]const u8 {
    var list = std.ArrayList(u8).init(allocator);
    defer {
        // if caller succeeds, they take ownership; otherwise free
        // However Zig defer cannot know; callers should dupe if needed.
    }
    const writer = list.writer();
    try t.format(writer);
    return list.toOwnedSlice();
}

/// Construct a function type with copied param array
pub fn makeFunctionType(allocator: std.mem.Allocator, params: []const Type, ret: Type) !Type {
    // Allocate copies of param types and store pointers
    const param_ptrs = try allocator.alloc(*const Type, params.len);
    var i: usize = 0;
    while (i < params.len) : (i += 1) {
        const tp = try allocator.create(Type);
        tp.* = params[i];
        param_ptrs[i] = tp;
    }
    // Allocate copy of return type
    const ret_ptr = try allocator.create(Type);
    ret_ptr.* = ret;

    // Allocate the function signature
    const sig = try allocator.create(FunctionSig);
    sig.* = .{ .param_types = param_ptrs, .return_type = ret_ptr };

    return Type{ .function = sig };
}

fn fnEqualsPtr(a: *const FunctionSig, b: *const FunctionSig) bool {
    if (a.param_types.len != b.param_types.len) return false;
    if (!Type.equals(a.return_type.*, b.return_type.*)) return false;
    var i: usize = 0;
    while (i < a.param_types.len) : (i += 1) {
        if (!Type.equals(a.param_types[i].*, b.param_types[i].*)) return false;
    }
    return true;
}
