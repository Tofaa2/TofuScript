// Trait cast + invoke demo (avoid nested call to match VM call semantics)
trait Printable {
  toString(self);
}

struct Point { x, y }

impl Printable for Point {
  toString(self) {
    return "Point(" + self.x + ", " + self.y + ")";
  }
}

func main() {
  var p = Point{ x: 3, y: 4 };
  var s = (p as Printable).toString();
  print(s);
}

main();