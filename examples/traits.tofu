// Trait system example - Phase 2 syntax
trait Printable;

// Define a struct with fields
struct Point { x, y };

// Implement trait for struct
impl Printable for Point;

// Regular program to show these declarations coexist:
func main() {
  var p = 0;
  print("Traits/structs/impl parsed; codegen pending");
}
main();