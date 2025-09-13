struct Point { x: Number, y: Number };

trait Printable {
  toString(self: Point): String;
}

impl Printable for Point {
  toString(self: Point) {
    return "(" + self.x + "," + self.y + ")";
  }
}

func main(): Nil {
  var p: Point = Point { x: 1, y: 2 };
  print((p as Printable).toString());
}

main();