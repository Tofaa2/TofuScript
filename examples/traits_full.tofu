// Traits like Rust and interfaces in Java — planned full syntax (not yet executable in Phase 2)
// This example shows the intended language surface for trait methods, impls,
// dynamic dispatch via trait-typed parameters, and usage in main.

// Declare traits with method requirements
trait Printable {
  toString(self);
}

trait Shape {
  area(self);
  draw(self);
}

// Declare structs with fields
struct Point { x, y }
struct Circle { r }

// Implement traits for structs with concrete method bodies
// impl Printable for Point {}

// Functions that accept trait-typed parameters (dynamic dispatch)
func printPrintable(p) {
  print("print");
}

func showShape(s) {
  print("show");
}

// Program entry point: construct values and use them via traits
func main() {
  var p = Point{ x: 3, y: 4 };
  var c = Circle{ r: 5 };

  printPrintable(p);  // Printable -> Point
  printPrintable(c);  // Printable -> Circle

  showShape(p);       // Shape -> Point
  showShape(c);       // Shape -> Circle
}

main();

// Notes:
// - This is a forward-looking example: method bodies, field access, object literals,
//   trait-typed parameters, and dynamic dispatch are not yet implemented in Phase 2.
// - The current compiler parses trait/struct/impl headers but ignores them in codegen.
// - This file documents the intended semantics and syntax to be implemented in later phases.