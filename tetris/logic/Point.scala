package tetris.logic

// you can alter this file!

case class Point(var x : Int, var y : Int) {


}

object Point{

   def add(p: Point, anchor: Point) : Point = {
    val res = Point(p.x,p.y)
    res.x += anchor.x
    res.y += anchor.y

    assert(res.x == p.x + anchor.x)
    assert(res.y == p.y + anchor.y)
    res
  }

  def centerRotateRight(p: Point): Point = {
    val res: Point = Point(-p.y, p.x)
    res
  }

  def centerRotateLeft(p: Point): Point = {
    val res: Point = Point(p.y, -p.x)
    res
  }

}