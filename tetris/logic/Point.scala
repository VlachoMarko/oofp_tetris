package tetris.logic

// you can alter this file!

case class Point(var x : Int, var y : Int) {
  var celltype : CellType = Empty
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
    Point(-p.y, p.x)
    }

  def centerRotateLeft(p: Point): Point = {
    Point(p.y, -p.x)
  }

  def iRotateRight(p: Point): Point = {
    Point(-p.y + 1, p.x)
  }
  def iRotateLeft(p: Point): Point ={
    Point(p.y, -p.x + 1)
  }

  def pDown(p: Point): Point = {
    val newp = Point(p.x, p.y+1)
    if (p.celltype != Empty) newp.celltype = p.celltype
    newp
  }

  def pLeft(p: Point): Point = {
    Point(p.x - 1, p.y)
  }

  def pRight(p: Point): Point = {
    Point(p.x + 1, p.y)
  }




}