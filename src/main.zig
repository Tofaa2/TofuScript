const std = @import("std");
const lexer = @import("lexer.zig");
const tokens = @import("tokens.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const compiler = @import("compiler.zig");
const vm = @import("vm.zig");
const value = @import("value.zig");
const trace = @import("trace.zig");
const resolver = @import("resolver.zig");
const typecheck = @import("typecheck.zig");
const types_mod = @import("types.zig");
const build_options = @import("build_options");

const Lexer = lexer.Lexer;
const Token = tokens.Token;
const Parser = parser.Parser;
const Node = ast.Node;
const Compiler = compiler.Compiler;
const VM = vm.VM;
const ObjNative = value.ObjNative;
const Value = value.Value;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    //defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // If a path is provided as the first CLI arg, read program text from that file.
    // Otherwise, run a built-in demo program.
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var source: []const u8 = undefined;
    if (args.len >= 2) {
        const path = args[1];
        source = try std.fs.cwd().readFileAlloc(allocator, path, 10 * 1024 * 1024);
    } else {
        source =
            \\ func main(): Nil {
            \\   var sum = 0;
            \\   for (var i = 0; i < 5; i = i + 1) {
            \\     if (i == 2) continue;
            \\     sum = sum + i;
            \\   }
            \\   var j = 0;
            \\   while (j < 2) {
            \\     sum = sum + 10;
            \\     j = j + 1;
            \\   }
            \\   if (sum >= 0) {
            \\     print(sum);
            \\   } else {
            \\     print(0);
            \\   }
            \\ }
            \\
            \\ main();
        ;
    }

    // Tokenize
    var my_lexer = Lexer.init(allocator, source);
    var token_list = try my_lexer.scanTokens();
    defer {
        for (token_list.items) |token| {
            token.deinit(allocator);
        }
        token_list.deinit();
    }

    // Parse
    var my_parser = Parser.init(allocator, token_list);
    const ast_root = try my_parser.parse();
    defer ast_root.deinit(allocator);

    // Phase 2.5: Resolver (nominal registration) and TypeChecker (strict/loose via -Dtypes)
    var res = try resolver.runResolve(allocator, ast_root);
    defer res.deinit(allocator);
    if (!res.ok) {
        for (res.diagnostics.items) |msg| {
            std.debug.print("{s}\n", .{msg});
        }
        return;
    }

    const mode: types_mod.Mode = if (std.mem.eql(u8, build_options.types_mode, "strict")) .strict else .loose;
    var tc_res = try typecheck.runTypecheck(allocator, ast_root, mode);
    defer tc_res.deinit(allocator);
    if (!tc_res.ok) {
        for (tc_res.diagnostics.items) |msg| {
            std.debug.print("{s}\n", .{msg});
        }
        return;
    }

    // Compile
    var my_compiler = try Compiler.init(allocator, .script);
    defer my_compiler.deinit();
    try my_compiler.compile(ast_root);

    if (trace.enabled) {
        my_compiler.function.chunk.disassemble("main");
    }

    // Add native functions
    var my_vm = try VM.init(allocator);
    defer my_vm.deinit();

    const print_native = try ObjNative.init(allocator, &nativePrint);
    try my_vm.globals.put(try allocator.dupe(u8, "print"), Value{ .obj = &print_native.obj });

    // Run
    try my_vm.interpret(my_compiler.function);

    // Disassemble the chunk (for debugging)
    if (trace.enabled) {
        my_compiler.function.chunk.disassemble("main");
    }
}

fn nativePrint(arg_count: u8, args: []value.Value) value.Value {
    for (args[0..arg_count]) |arg| {
        arg.print();
        std.debug.print(" ", .{});
    }
    if (trace.enabled) trace.log("NATIVE", "print called argc={d}", .{arg_count});
    std.debug.print("\n", .{});
    return value.Value{ .nil = {} };
}
