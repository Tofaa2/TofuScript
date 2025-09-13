// Trait cast + invoke demo (avoid nested call to match VM call semantics)
trait Printable {
  toString(self: Point): String;
}

struct Point { x: Number, y: Number }

impl Printable for Point {
  toString(self: Point): String {
    return "Point(" + self.x + ", " + self.y + ")";
  }
}

func main(): Nil {
  var p = Point{ x: 3, y: 4 };
  var s = (p as Printable).toString();
  print(s);
}

main();