const std = @import("std");
const ast = @import("ast.zig");
const types = @import("types.zig");

const Node = ast.Node;

pub const TypeChecker = struct {
    allocator: std.mem.Allocator,
    mode: types.Mode, // retained for compatibility; treated as strict
    diagnostics: std.ArrayList([]const u8),

    // Scopes: name -> Type
    scopes: std.ArrayList(std.StringHashMap(types.Type)),
    // Struct fields registry: struct_name -> (field_name -> Type)
    struct_fields: std.StringHashMap(std.StringHashMap(types.Type)),
    // Function signatures registry: func_name -> *const FunctionSig
    func_sigs: std.StringHashMap(*const types.FunctionSig),

    // Current function return type (if in function)
    current_return: ?types.Type = null,

    pub fn init(allocator: std.mem.Allocator, mode: types.Mode) TypeChecker {
        _ = mode; // unused (strict enforced)
        var tc = TypeChecker{
            .allocator = allocator,
            .mode = .strict, // force strict regardless of provided mode
            .diagnostics = std.ArrayList([]const u8).init(allocator),
            .scopes = std.ArrayList(std.StringHashMap(types.Type)).init(allocator),
            .struct_fields = std.StringHashMap(std.StringHashMap(types.Type)).init(allocator),
            .func_sigs = std.StringHashMap(*const types.FunctionSig).init(allocator),
            .current_return = null,
        };
        // push global scope
        const global = std.StringHashMap(types.Type).init(allocator);
        tc.scopes.append(global) catch unreachable;
        return tc;
    }

    pub fn deinit(self: *TypeChecker) void {
        for (self.diagnostics.items) |msg| self.allocator.free(msg);

        // Deinit scopes
        var i: usize = 0;
        while (i < self.scopes.items.len) : (i += 1) {
            self.scopes.items[i].deinit();
        }
        self.scopes.deinit();

        // Deinit struct fields registry
        var it = self.struct_fields.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.deinit();
        }
        self.struct_fields.deinit();

        // func_sigs values contain slices allocated by makeFunctionType; we do not deeply free them here.
        self.func_sigs.deinit();

        self.diagnostics.deinit();
    }

    pub fn check(self: *TypeChecker, root: *Node) anyerror!void {
        if (root.type != .program) return;

        // Phase 1: collect struct field type maps
        for (root.data.program.body.items) |s| {
            if (s.type == .struct_decl) {
                try self.registerStructFields(s);
            }
        }

        // Phase 2: collect function signatures (names and param/return types)
        for (root.data.program.body.items) |s| {
            if (s.type == .function_decl) {
                try self.registerFunctionSignature(s);
            }
        }

        // Phase 3: semantic checking
        for (root.data.program.body.items) |stmt| {
            try self.checkNode(stmt);
        }
    }

    fn pushScope(self: *TypeChecker) void {
        const scope = std.StringHashMap(types.Type).init(self.allocator);
        self.scopes.append(scope) catch unreachable;
    }

    fn popScope(self: *TypeChecker) void {
        var scope = self.scopes.pop().?;
        scope.deinit();
    }

    fn declare(self: *TypeChecker, name: []const u8, ty: types.Type) !void {
        const scope = &self.scopes.items[self.scopes.items.len - 1];
        // store a dup of name for stability
        try scope.put(try self.allocator.dupe(u8, name), ty);
    }

    fn lookup(self: *TypeChecker, name: []const u8) ?types.Type {
        var i: isize = @as(isize, @intCast(self.scopes.items.len)) - 1;
        while (i >= 0) : (i -= 1) {
            const scope = self.scopes.items[@intCast(i)];
            if (scope.get(name)) |t| return t;
        }
        return null;
    }

    fn typeFromAnnotation(_: *TypeChecker, ann: ?[]const u8) types.Type {
        if (ann) |s| {
            return types.nominalFromIdent(s);
        }
        return .unknown;
    }

    fn registerStructFields(self: *TypeChecker, node: *Node) !void {
        const sd = node.data.struct_decl;
        var fields_map = std.StringHashMap(types.Type).init(self.allocator);
        var i: usize = 0;
        while (i < sd.fields.items.len) : (i += 1) {
            const fname = sd.fields.items[i];
            const f_ann = sd.field_types.items[i];
            const f_ty = self.typeFromAnnotation(f_ann);
            try fields_map.put(try self.allocator.dupe(u8, fname), f_ty);
        }
        try self.struct_fields.put(try self.allocator.dupe(u8, sd.name), fields_map);
    }

    fn registerFunctionSignature(self: *TypeChecker, node: *Node) !void {
        const f = node.data.function_decl;
        // Build parameter types array
        const param_count = f.params.items.len;
        var tmp = try self.allocator.alloc(types.Type, param_count);
        var i: usize = 0;
        while (i < param_count) : (i += 1) {
            tmp[i] = self.typeFromAnnotation(f.param_types.items[i]);
        }
        const ret_ty = self.typeFromAnnotation(f.return_type);
        const sig_ty = try types.makeFunctionType(self.allocator, tmp, ret_ty);
        // Also declare in global scope as a value
        try self.declare(f.name, sig_ty);
        // Save signature pointer by name
        try self.func_sigs.put(try self.allocator.dupe(u8, f.name), sig_ty.function);
    }

    fn checkNode(self: *TypeChecker, node: *Node) anyerror!void {
        switch (node.type) {
            .function_decl => try self.checkFunction(node),
            .struct_decl => try self.checkStruct(node),
            .trait_decl => try self.checkTrait(node),
            .impl_decl => try self.checkImpl(node),
            .variable_decl => try self.checkVar(node),
            .block_stmt => {
                self.pushScope();
                defer self.popScope();
                for (node.data.block_stmt.statements.items) |s| {
                    try self.checkNode(s);
                }
            },
            .if_stmt => {
                const cond_ty = try self.checkExpr(node.data.if_stmt.condition);
                if (!self.expectBool(cond_ty)) {
                    try self.errFmt("If condition must be Bool", .{});
                }
                try self.checkNode(node.data.if_stmt.then_branch);
                if (node.data.if_stmt.else_branch) |e| try self.checkNode(e);
            },
            .while_stmt => {
                const cond_ty = try self.checkExpr(node.data.while_stmt.condition);
                if (!self.expectBool(cond_ty)) {
                    try self.errFmt("While condition must be Bool", .{});
                }
                try self.checkNode(node.data.while_stmt.body);
            },
            .for_stmt => {
                if (node.data.for_stmt.initializer) |i| try self.checkNode(i);
                if (node.data.for_stmt.condition) |c| {
                    const cond_ty = try self.checkExpr(c);
                    if (!self.expectBool(cond_ty)) {
                        try self.errFmt("For condition must be Bool", .{});
                    }
                }
                if (node.data.for_stmt.increment) |inc| {
                    _ = try self.checkExpr(inc);
                }
                try self.checkNode(node.data.for_stmt.body);
            },
            .expression_stmt => {
                _ = try self.checkExpr(node.data.expression_stmt.expr);
            },
            .assignment => {
                const rhs_ty = try self.checkExpr(node.data.assignment.value);
                if (self.lookup(node.data.assignment.name)) |lhs_ty| {
                    if (!types.Type.isAssignable(lhs_ty, rhs_ty)) {
                        const lt = try types.typeToString(self.allocator, lhs_ty);
                        const rt = try types.typeToString(self.allocator, rhs_ty);
                        defer {
                            self.allocator.free(lt);
                            self.allocator.free(rt);
                        }
                        try self.errFmt("Cannot assign {s} to variable '{s}' of type {s}", .{ rt, node.data.assignment.name, lt });
                    }
                } else {
                    try self.errFmt("Undeclared variable '{s}'", .{node.data.assignment.name});
                }
            },
            .member_assign => {
                _ = try self.checkExpr(node.data.member_assign.object);
                _ = try self.checkExpr(node.data.member_assign.value);
                // For Phase 2.5, we do not enforce member assignment types yet.
            },
            .return_stmt => {
                if (self.current_return) |ret_expected| {
                    if (node.data.return_stmt.value) |v| {
                        const rt = try self.checkExpr(v);
                        if (!types.Type.isAssignable(ret_expected, rt)) {
                            const et = try types.typeToString(self.allocator, ret_expected);
                            const at = try types.typeToString(self.allocator, rt);
                            defer {
                                self.allocator.free(et);
                                self.allocator.free(at);
                            }
                            try self.errFmt("Return type mismatch: expected {s}, got {s}", .{ et, at });
                        }
                    } else {
                        // return; without value must match Nil
                        if (!(ret_expected == .builtin and ret_expected.builtin == .Nil)) {
                            const et = try types.typeToString(self.allocator, ret_expected);
                            defer self.allocator.free(et);
                            try self.errFmt("Return type mismatch: expected {s}, got Nil", .{et});
                        }
                    }
                } else {
                    try self.errFmt("Return statement not inside a function", .{});
                }
            },
            .program => {
                for (node.data.program.body.items) |s| try self.checkNode(s);
            },
            else => {},
        }
    }

    fn checkFunction(self: *TypeChecker, node: *Node) anyerror!void {
        const fn_data = node.data.function_decl;

        // Enforce annotations in strict mode
        if (fn_data.param_types.items.len != fn_data.params.items.len) {
            try self.errFmt("Function '{s}': internal error param/types length mismatch", .{fn_data.name});
        }
        // Require param annotations
        for (fn_data.param_types.items, 0..) |pt, i| {
            if (pt == null) {
                try self.errFmt("Function '{s}': missing type annotation for parameter {d} ('{s}')", .{ fn_data.name, i, fn_data.params.items[i] });
            }
        }
        if (fn_data.return_type == null) {
            try self.errFmt("Function '{s}': missing return type annotation", .{fn_data.name});
        }

        // Enter function scope
        self.pushScope();
        defer self.popScope();

        // Bind params
        var i: usize = 0;
        while (i < fn_data.params.items.len) : (i += 1) {
            const nm = fn_data.params.items[i];
            const ty = self.typeFromAnnotation(fn_data.param_types.items[i]);
            try self.declare(nm, ty);
        }

        // Set current return
        const ret_ty = self.typeFromAnnotation(fn_data.return_type);
        const save = self.current_return;
        self.current_return = ret_ty;
        defer self.current_return = save;

        // Body
        try self.checkNode(fn_data.body);
    }

    fn checkStruct(self: *TypeChecker, node: *Node) anyerror!void {
        const sd = node.data.struct_decl;
        // Enforce annotations for fields
        if (sd.field_types.items.len != sd.fields.items.len) {
            try self.errFmt("Struct '{s}': internal error fields/types length mismatch", .{sd.name});
        } else {
            var i: usize = 0;
            while (i < sd.fields.items.len) : (i += 1) {
                if (sd.field_types.items[i] == null) {
                    try self.errFmt("Struct '{s}': missing type annotation for field '{s}'", .{ sd.name, sd.fields.items[i] });
                }
            }
        }
    }

    fn checkTrait(self: *TypeChecker, node: *Node) anyerror!void {
        const td = node.data.trait_decl;
        // Enforce method annotations
        for (td.methods.items) |m| {
            if (m.param_types.items.len != m.params.items.len) {
                try self.errFmt("Trait '{s}': method '{s}' param/types mismatch", .{ td.name, m.name });
                continue;
            }
            var i: usize = 0;
            while (i < m.params.items.len) : (i += 1) {
                if (m.param_types.items[i] == null) {
                    try self.errFmt("Trait '{s}': method '{s}' missing type for parameter {d} ('{s}')", .{ td.name, m.name, i, m.params.items[i] });
                }
            }
            if (m.return_type == null) {
                try self.errFmt("Trait '{s}': method '{s}' missing return type", .{ td.name, m.name });
            }
        }
    }

    fn checkImpl(self: *TypeChecker, node: *Node) anyerror!void {
        _ = self;
        _ = node;
        // Conformance checks can be added later.
        return;
    }

    fn checkVar(self: *TypeChecker, node: *Node) anyerror!void {
        const vd = node.data.variable_decl;

        var declared: types.Type = .unknown;
        if (vd.declared_type) |dt| {
            declared = self.typeFromAnnotation(dt);
        }

        var init_ty: types.Type = .unknown;
        if (vd.value) |v| {
            init_ty = try self.checkExpr(v);
        }

        if (declared == .unknown and init_ty == .unknown) {
            try self.errFmt("Variable '{s}': missing type annotation or initializer in strict mode", .{vd.name});
            // Still declare as Unknown to avoid cascading "undeclared" errors in the same scope.
            try self.declare(vd.name, types.Type{ .unknown = {} });
            return;
        }

        const final_ty = if (declared != .unknown) blk: {
            if (init_ty != .unknown and !types.Type.isAssignable(declared, init_ty)) {
                const dt = try types.typeToString(self.allocator, declared);
                const it = try types.typeToString(self.allocator, init_ty);
                defer {
                    self.allocator.free(dt);
                    self.allocator.free(it);
                }
                try self.errFmt("Variable '{s}': initializer type {s} not assignable to declared type {s}", .{ vd.name, it, dt });
            }
            break :blk declared;
        } else init_ty;

        try self.declare(vd.name, final_ty);
    }

    fn expectBool(_: *TypeChecker, t: types.Type) bool {
        return t == .builtin and t.builtin == .Bool;
    }

    fn expectNumber(_: *TypeChecker, t: types.Type) bool {
        return t == .builtin and t.builtin == .Number;
    }

    fn expectString(_: *TypeChecker, t: types.Type) bool {
        return t == .builtin and t.builtin == .String;
    }

    fn checkExpr(self: *TypeChecker, node: *Node) anyerror!types.Type {
        switch (node.type) {
            .literal => {
                const lv = node.data.literal.value;
                return switch (lv) {
                    .number => types.Type{ .builtin = .Number },
                    .string => types.Type{ .builtin = .String },
                    .boolean => types.Type{ .builtin = .Bool },
                    .nil => types.Type{ .builtin = .Nil },
                };
            },
            .identifier => {
                const name = node.data.identifier.name;

                // Built-in 'print' is a native; treat as a function with Nil return for type checking.
                if (std.mem.eql(u8, name, "print")) {
                    // zero-parameter function returning Nil (call_expr has special-case handling for args)
                    const empty = try self.allocator.alloc(types.Type, 0);
                    defer self.allocator.free(empty);
                    const ret_nil = types.Type{ .builtin = .Nil };
                    return try types.makeFunctionType(self.allocator, empty, ret_nil);
                }

                if (self.lookup(name)) |t| return t else {
                    try self.errFmt("Use of undeclared identifier '{s}'", .{name});
                    return .unknown;
                }
            },
            .unary_expr => {
                const op = node.data.unary_expr.operator.lexeme;
                const rt = try self.checkExpr(node.data.unary_expr.right);
                if (std.mem.eql(u8, op, "!")) {
                    if (!self.expectBool(rt)) {
                        try self.errFmt("Unary '!' expects Bool, got other", .{});
                    }
                    return types.Type{ .builtin = .Bool };
                } else if (std.mem.eql(u8, op, "-")) {
                    if (!self.expectNumber(rt)) {
                        try self.errFmt("Unary '-' expects Number", .{});
                    }
                    return types.Type{ .builtin = .Number };
                } else {
                    return .unknown;
                }
            },
            .binary_expr => {
                const l = try self.checkExpr(node.data.binary_expr.left);
                const r = try self.checkExpr(node.data.binary_expr.right);
                const op = node.data.binary_expr.operator.lexeme;

                if (std.mem.eql(u8, op, "+")) {
                    // Number + Number -> Number
                    if (self.expectNumber(l) and self.expectNumber(r)) {
                        return types.Type{ .builtin = .Number };
                    }
                    // String + String -> String
                    if (self.expectString(l) and self.expectString(r)) {
                        return types.Type{ .builtin = .String };
                    }
                    // String + Number or Number + String -> String (coercion)
                    if ((self.expectString(l) and self.expectNumber(r)) or (self.expectNumber(l) and self.expectString(r))) {
                        return types.Type{ .builtin = .String };
                    }
                    try self.errFmt("Operator '+' not defined for given operand types", .{});
                    return .unknown;
                }

                if (std.mem.eql(u8, op, "-") or std.mem.eql(u8, op, "*") or std.mem.eql(u8, op, "/")) {
                    if (!(self.expectNumber(l) and self.expectNumber(r))) {
                        try self.errFmt("Arithmetic operator expects Number operands", .{});
                    }
                    return types.Type{ .builtin = .Number };
                }

                if (std.mem.eql(u8, op, "==") or std.mem.eql(u8, op, "!=")) {
                    // For now, allow any comparison; result is Bool
                    return types.Type{ .builtin = .Bool };
                }

                if (std.mem.eql(u8, op, ">") or std.mem.eql(u8, op, "<") or std.mem.eql(u8, op, ">=") or std.mem.eql(u8, op, "<=") or
                    std.mem.eql(u8, op, ">") or std.mem.eql(u8, op, "<") or std.mem.eql(u8, op, ">=") or std.mem.eql(u8, op, "<="))
                {
                    if (!(self.expectNumber(l) and self.expectNumber(r))) {
                        try self.errFmt("Relational operator expects Number operands", .{});
                    }
                    return types.Type{ .builtin = .Bool };
                }

                return .unknown;
            },
            .logical_expr => {
                const lt = try self.checkExpr(node.data.logical_expr.left);
                if (!self.expectBool(lt)) try self.errFmt("Logical operator expects Bool operands (left)", .{});
                const rt = try self.checkExpr(node.data.logical_expr.right);
                if (!self.expectBool(rt)) try self.errFmt("Logical operator expects Bool operands (right)", .{});
                return types.Type{ .builtin = .Bool };
            },
            .call_expr => {
                // Special-case native print: allow any arguments, returns Nil, and skip arity/type checks.
                if (node.data.call_expr.callee.type == .identifier) {
                    const nm = node.data.call_expr.callee.data.identifier.name;
                    if (std.mem.eql(u8, nm, "print")) {
                        // Type-check arguments for traversal side-effects, ignore results
                        var i_print: usize = 0;
                        while (i_print < node.data.call_expr.args.items.len) : (i_print += 1) {
                            _ = try self.checkExpr(node.data.call_expr.args.items[i_print]);
                        }
                        return types.Type{ .builtin = .Nil };
                    }
                }

                const callee_ty = try self.checkExpr(node.data.call_expr.callee);
                // Check arguments
                var args_types = try self.allocator.alloc(types.Type, node.data.call_expr.args.items.len);
                defer self.allocator.free(args_types);
                var i: usize = 0;
                while (i < node.data.call_expr.args.items.len) : (i += 1) {
                    args_types[i] = try self.checkExpr(node.data.call_expr.args.items[i]);
                }

                if (@as(std.meta.Tag(types.Type), callee_ty) == .function) {
                    const sigp = callee_ty.function; // *const FunctionSig
                    const sig = sigp.*;
                    if (sig.param_types.len != args_types.len) {
                        try self.errFmt("Function call arity mismatch: expected {d}, got {d}", .{ sig.param_types.len, args_types.len });
                    } else {
                        var j: usize = 0;
                        while (j < sig.param_types.len) : (j += 1) {
                            if (!types.Type.isAssignable(sig.param_types[j].*, args_types[j])) {
                                const et = try types.typeToString(self.allocator, sig.param_types[j].*);
                                const at = try types.typeToString(self.allocator, args_types[j]);
                                defer {
                                    self.allocator.free(et);
                                    self.allocator.free(at);
                                }
                                try self.errFmt("Call argument {d}: expected {s}, got {s}", .{ j, et, at });
                            }
                        }
                    }
                    return sig.return_type.*;
                } else {
                    // If function by name exists, use that signature
                    // This helps when callee is an identifier with a registered signature.
                    if (node.data.call_expr.callee.type == .identifier) {
                        const nm = node.data.call_expr.callee.data.identifier.name;
                        if (self.func_sigs.get(nm)) |sig2p| {
                            const sig2 = sig2p.*;
                            if (sig2.param_types.len != args_types.len) {
                                try self.errFmt("Function call arity mismatch: expected {d}, got {d}", .{ sig2.param_types.len, args_types.len });
                            } else {
                                var k: usize = 0;
                                while (k < sig2.param_types.len) : (k += 1) {
                                    if (!types.Type.isAssignable(sig2.param_types[k].*, args_types[k])) {
                                        const et = try types.typeToString(self.allocator, sig2.param_types[k].*);
                                        const at = try types.typeToString(self.allocator, args_types[k]);
                                        defer {
                                            self.allocator.free(et);
                                            self.allocator.free(at);
                                        }
                                        try self.errFmt("Call argument {d}: expected {s}, got {s}", .{ k, et, at });
                                    }
                                }
                            }
                            return sig2.return_type.*;
                        }
                    }
                    // Unknown callee type; return Unknown
                    return .unknown;
                }
            },
            .member_expr => {
                const obj_ty = try self.checkExpr(node.data.member_expr.object);
                if (@as(std.meta.Tag(types.Type), obj_ty) == .user) {
                    const sname = obj_ty.user;
                    if (self.struct_fields.get(sname)) |fields| {
                        if (fields.get(node.data.member_expr.field)) |fty| {
                            return fty;
                        } else {
                            try self.errFmt("Unknown field '{s}' on struct '{s}'", .{ node.data.member_expr.field, sname });
                            return .unknown;
                        }
                    } else {
                        // Unknown nominal; allow but unknown type
                        return .unknown;
                    }
                }
                // For trait instances or other, we do not type fields at Phase 2.5
                return .unknown;
            },
            .struct_lit => {
                const sname = node.data.struct_lit.type_name;
                if (self.struct_fields.get(sname)) |fields| {
                    // Validate provided inits against struct field types (when present in map)
                    for (node.data.struct_lit.inits.items) |fi| {
                        const ety = try self.checkExpr(fi.expr);
                        if (fields.get(fi.name)) |fty| {
                            if (!types.Type.isAssignable(fty, ety)) {
                                const ft = try types.typeToString(self.allocator, fty);
                                const at = try types.typeToString(self.allocator, ety);
                                defer {
                                    self.allocator.free(ft);
                                    self.allocator.free(at);
                                }
                                try self.errFmt("Struct '{s}': field '{s}' expects {s}, got {s}", .{ sname, fi.name, ft, at });
                            }
                        }
                    }
                }
                return types.Type{ .user = sname };
            },
            .trait_cast => {
                // Accept any for now
                _ = try self.checkExpr(node.data.trait_cast.object);
                return .unknown;
            },
            .trait_invoke => {
                // Validate target expression and args
                _ = try self.checkExpr(node.data.trait_invoke.target);
                for (node.data.trait_invoke.args.items) |a| {
                    _ = try self.checkExpr(a);
                }
                return .unknown;
            },
            else => return .unknown,
        }
    }

    fn errFmt(self: *TypeChecker, comptime fmt: []const u8, args: anytype) anyerror!void {
        const msg = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.diagnostics.append(msg);
    }
};

pub const TypecheckResult = struct {
    ok: bool,
    diagnostics: std.ArrayList([]const u8),

    pub fn deinit(self: *TypecheckResult, allocator: std.mem.Allocator) void {
        for (self.diagnostics.items) |msg| allocator.free(msg);
        self.diagnostics.deinit();
    }
};

pub fn runTypecheck(allocator: std.mem.Allocator, root: *Node, mode: types.Mode) !TypecheckResult {
    var tc = TypeChecker.init(allocator, mode);
    defer tc.deinit();

    try tc.check(root);

    var diags = std.ArrayList([]const u8).init(allocator);
    for (tc.diagnostics.items) |msg| {
        try diags.append(try allocator.dupe(u8, msg));
    }

    return .{
        .ok = diags.items.len == 0,
        .diagnostics = diags,
    };
}
