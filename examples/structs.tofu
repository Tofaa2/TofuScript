// Struct example - Phase 2 structs implemented
struct Point { x, y }

func main() {
  var p = Point{ x: 3, y: 4 };
  print("Point: (" + p.x + ", " + p.y + ")");
  p.x = 10;
  print("After change: (" + p.x + ", " + p.y + ")");
}

main();