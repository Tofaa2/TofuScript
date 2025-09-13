# TofuScript

A small, experimental bytecode-based scripting language implemented in Zig 0.14.0. This repository currently provides a lexer, parser, bytecode compiler, and a simple stack-based VM with native interop.

Status: early prototype (WIP). The README reflects current capabilities and a concrete plan to reach a performant, production-ready language.

## Quick start

Prerequisites: Zig 0.14.0+

Build and run:

```bash
zig build
zig build run
zig build test
```

Minimal program supported today:

```txt
func main() {
  var x = 10;
  var y = 20;
  print(x + y);
}

main();
```

## Tracing and disassembly

Structured tracing is available behind a compile-time flag. When enabled, the VM, compiler, and bytecode layers emit concise, structured logs, and the main program disassembles the compiled chunk.

Enable tracing:
```bash
zig build -Dtrace=true run
```

What you get when tracing is enabled:
- VM step trace: instruction pointer and opcode logs via [trace.log()](src/trace.zig:8)
- Compiler and bytecode events (e.g., constants added/emitted)
- Disassembly output from [Chunk.disassemble()](src/bytecode.zig:74) gated in [main()](src/main.zig:20)
## Current capabilities (truthful)

Language and parser:
- Functions: declaration with name and parameters; calls with up to 255 args.
- Variables: global var declarations with optional initializer; assignment to identifiers.
- Blocks and statements: lexical blocks with braces; if/else; while; for (C-style clauses); return; break; continue.
- Operators: literals (number, string, bool, nil), + - * /, == != < <= > >=, logical and/or (short-circuit), unary ! and -, grouping, calls, identifiers.
- Print: print statement desugared to a function call.
- Imports: recognized syntax; currently ignored by the compiler.
- Parser recovery: synchronize on semicolons and braces to continue after errors.

Compiler and bytecode:
- Emits stack-oriented bytecode with short and long constant forms.
- Globals only: get_global, set_global, define_global; no locals or closures yet.
- Control flow lowering: if/else, while, for via jump, jump_if_false, loop; expression statements emit pop; logical and/or short-circuit codegen.
- Function objects with arity; calls and returns.
- Opcodes implemented: constant, constant_long, get_global, set_global, define_global, add, subtract, multiply, divide, negate, equal, greater, less, jump, jump_if_false, loop, call, return, print, pop, nil, true, false, not.

Virtual machine:
- Stack-based VM with call frames, simple bounds checks, and native function interop (e.g., print).
- Values: number, boolean, nil, string object, function object, native function object.
- Strings support concatenation; interning is not yet implemented.
- Control flow: supports conditional jumps, loops, and correct break/continue behavior consistent with compiler lowering.

## Architecture

TofuScript follows a traditional pipeline:

```
Source → Lexer → Parser → AST → Compiler → Bytecode → VM
```

Where we are headed:
- Insert Resolver between Parser and Compiler to bind identifiers, manage locals/upvalues, and support closures.
- Insert TypeChecker after Resolver to enforce static types, annotate AST, and enable better codegen and error reporting.
- Introduce precise GC and central allocator for all heap objects.
- Add module loader before compilation for multi-file programs.

Mermaid overview:

```mermaid
flowchart TD
  A[Source] --> B[Lexer]
  B --> C[Parser]
  C --> D[AST]
  D --> E[Resolver]
  E --> F[TypeChecker]
  F --> G[Compiler]
  G --> H[Bytecode]
  H --> I[VM]
```

## Roadmap to 1.0

Phase 0 (COMPLETE) — Stabilize the prototype:
- Replace debug prints with structured tracing behind a compile-time flag.
- Improve error messages and add parser error synchronization.
- Add disassembler improvements for bytecode debugging.

Phase 1 (COMPLETE) — Language completeness (MVP):
- Statements: if/else, while, for, return, break/continue.
- Operators: logical and/or, proper unary nodes.
- Parser recovery: synchronize on ; and braces to continue after errors.

Phase 2 — (Complete) Scoping and closures:
- Implement Resolver: scopes, locals, parameters-as-locals, shadowing rules.
- VM opcodes: LOAD_LOCAL, STORE_LOCAL, OP_CLOSURE, OP_GET_UPVALUE, OP_SET_UPVALUE.
- Introduction of structs, traits and trait implementation for structs.
- Closure objects and upvalue capture semantics.

Phase 2.5 (Complete) — Static type system foundations:
- Syntax: explicit type annotations for variables, parameters, returns, struct fields. Examples:
  - var x: Number = 42;
  - func len(a: String): Number { ... }
  - struct Point { x: Number, y: Number }
- Resolver extensions: bind identifiers with type info; build a typed symbol table.
- TypeChecker: annotate AST with types; validate operations; produce helpful errors.
- Local inference: infer types of let/var initializers within a statement where unambiguous (no global/field inference initially).
- Traits and types: treat traits as interfaces; method signatures carry types; impl conformance checks.
- Codegen policy: keep current bytecode untyped but reject ill-typed programs at compile time; prepare for typed-specialized fast paths.
- Nativisation policy: keep primitive types native to the project itself, not the std. Convert print to a native function and have native function access instead of it littering the ast. Make primitives consistent with naming.

Phase 3 — Runtime and memory:
- Central allocator for all heap objects.
- Precise mark-sweep GC with root set (stack, globals, frames), write barriers, and stress tests.
- String interning and rope/arena strategy for concat-heavy workloads.

Phase 4 — Collections and standard library MVP (typed):
- Arrays: NEW_ARRAY, GET_INDEX, SET_INDEX; iterators; element type tracked by TypeChecker.
- Maps: NEW_MAP, GET_KEY, SET_KEY; typed keys/values; hashing for strings and numbers.
- stdlib: io, os, math, string, array, map, fmt with typed signatures.

Phase 5 — Tooling and UX:
- REPL with line editing, multi-line input, and error reporting.
- Module system: file-based loader, search paths, module cache.
- LSP and Syntax Highlighting (type-aware diagnostics, hover types, go-to def).
- Debugger hooks: breakpoints, step, stack traces, value inspection.

Phase 6 — Optimization and releases:
- Compiler: constant folding, dead-code elimination for blocks, peephole pass (type-aware).
- VM: inline caches for globals and calls, superinstructions for hot opcode pairs, fast paths for numbers and common typed ops.
- Cross-platform CI and prebuilt binaries for Windows/macOS/Linux.

## Performance plan

Short term (unlock obvious wins):
- Remove per-step debug printing in non-debug builds; add a trace opcode flag.
- Bounds-check elision in hot paths with assertions in Debug, checks in ReleaseSafe.
- Value representation: ensure numbers use unboxed f64; consider NaN-tagging as a later optimization.
- String interning table to remove duplicate allocations and speed equality.

Medium term (algorithmic improvements):
- Introduce inline caches for get_global and set_global, and monomorphic call sites.
- Add superinstructions for common pairs like load-constant; call; return and arithmetic chains.
- Improve constant pool layout and constant_long emission to be branchless.
- Basic block formation in compiler to aid peephole optimizer.
- Type-guided optimizations: constant-propagate types; remove dynamic tag checks for statically safe regions.

Long term (memory and locality):
- Precise GC with generational nursery; bump allocation for short-lived objects.
- Allocate frames and stacks in contiguous arenas to improve cache locality.
- Consider optional register-based VM variant behind build flag, measured by benchmarks.
- Optional typed-specialized opcodes for hottest numeric and string paths (behind a build flag).

Benchmarking strategy:
- Micro: arithmetic, string concat, function call overhead, map/array ops once implemented.
- Macro: Fibonacci, JSON decode/encode (post-stdlib), regex-lite workloads if added later.
- Harness: run in zig build benchmark with warmup, median-of-N, CI thresholds to prevent regressions.

## Testing and CI

- Unit tests per component; add golden tests for lexer/parser and VM traces.
- TypeChecker tests: well-typed acceptance, ill-typed rejection, error spans, and trait conformance tests.
- End-to-end tests from source to output for small typed programs.
- GitHub Actions: build, test, format, and run benchmarks on push/PR across platforms.

## Contributing

Contributions are welcome. Good first issues will be labeled as we break work into small tasks. Please discuss larger changes in issues first.

## License

MIT License. See LICENSE.

## Static typing vision and plan

Why static types
- Predictable performance: eliminate dynamic tag checks in verified regions; unlock typed fast paths in VM and compiler.
- Maintainability: explicit contracts at function boundaries; earlier and clearer error messages; safer refactors.
- Tooling: richer IDE support and documentation from types.

Surface syntax (initial)
- Variables: var x: Number = 42; var s: String = "hi";
- Functions: func add(a: Number, b: Number): Number { ... }
- Structs: struct Point { x: Number, y: Number }
- Traits: trait Printable { toString(self: Self): String }
- Impl: impl Printable for Point { toString(self: Point): String { ... } }

Semantics and inference
- Strongly typed by default: annotations required at public boundaries (func params/returns, struct fields). Local inference allowed for simple initializers: var x = 1; infers Number.
- No implicit widening that loses precision. Numeric ops require compatible operands; string + number is allowed via ToString coercion rule or explicit fmt, to be decided behind a feature flag.
- Trait conformance checked at compile time: implemented methods must match signatures.

Compiler architecture changes
- Resolver: extended to record symbol kinds and declared types.
- TypeChecker: runs after Resolver to annotate AST, resolve generics (future), and verify trait impls.
- Bytecode/VM: remain untyped initially; rely on TypeChecker for safety. Later, introduce typed fast paths where profitable.

Phased delivery
- Phase 2.5 (this roadmap): syntax, Resolver extensions, TypeChecker for core types, typed traits/impls, local inference.
- Phase 3 tie-in: allocator/GC aware of typed objects where helpful; string interning supports fast equality aligned with String type.
- Phase 4 tie-in: typed collections; iterators expose element types; stdlib APIs exported with signatures.

Migration strategy
- Gate enforcement behind a build flag initially: -Dtypes=strict vs -Dtypes=loose.
- Start by annotating stdlib and examples; provide compiler suggestions for missing annotations.
- Add formatter hints and LSP integration to auto-insert stubs.

Open questions to validate via RFCs
- Numeric tower and literal typing (Number vs Int/Float)
- Generics and monomorphization vs reified generics
- Nullability model: Option[T] vs implicit nil
- String interpolation and ToString rules