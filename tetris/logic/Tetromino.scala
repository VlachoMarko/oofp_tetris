package tetris.logic

import tetris.logic.Point.{add, centerRotateLeft, centerRotateRight}
import tetris.logic.Tetromino.{getBodyBlocks, setBlocksAndType}

abstract class Tetromino {

  var bodyBlocks : Vector[Point]
  var blockType : CellType = Empty
  var anchor : Point

  final def JBody: (Point, Point, Point) = (Point(-1,-1), Point(-1,0), Point(1,0))
  final def LBody: (Point, Point, Point) = (Point(-1,0), Point(1,0), Point(1,-1))
  final def SBody: (Point, Point, Point) = (Point(-1,0), Point(0,-1), Point(1,-1))
  final def TBody: (Point, Point, Point) = (Point(-1,0), Point(0,-1), Point(1,0))
  final def ZBody: (Point, Point, Point) = (Point(-1,-1), Point(0,-1), Point(1,0))


  def rotateLeft(): Unit
  def rotateRight(): Unit

}

class centeredTetromino(randomType: Int, override var anchor: Point) extends Tetromino {
  var bodyBlocks: Vector[Point] = Vector[Point]()
  setVars()


  override def rotateLeft(): Unit = {
    bodyBlocks = bodyBlocks.map(centerRotateLeft(anchor))
    println("we here1: " + bodyBlocks)
  }

  override def rotateRight(): Unit = {
    bodyBlocks = bodyBlocks.map(centerRotateRight(anchor))
    println("we here2: " + bodyBlocks)
  }


  def setVars(): Unit = {
    randomType match {
      case 1 => setBlocksAndType(this, JCell, getBodyBlocks(super.JBody, anchor))
      case 2 => setBlocksAndType(this, LCell, getBodyBlocks(super.LBody, anchor))
      case 4 => setBlocksAndType(this, SCell, getBodyBlocks(super.SBody, anchor))
      case 5 => setBlocksAndType(this, TCell, getBodyBlocks(super.TBody, anchor))
      case 6 => setBlocksAndType(this, ZCell, getBodyBlocks(super.ZBody, anchor))
    }
  }


}

class oTetromino(override var anchor: Point) extends Tetromino {

  private def oBody: (Point, Point, Point) = (Point(0, -1), Point(1, -1), Point(1, 0))
  var bodyBlocks: Vector[Point] = getBodyBlocks(oBody, anchor)
  blockType = OCell

  def rotateLeft(): Unit = ()
  def rotateRight(): Unit = ()
}

class iTetromino(override var anchor: Point) extends Tetromino {

  private def iBody: (Point, Point, Point) = (Point(-1,0), Point(1,0), Point(2,0))

  var bodyBlocks: Vector[Point] = getBodyBlocks(iBody, anchor)
  blockType = ICell


  def rotateLeft(): Unit = {
  }

  def rotateRight(): Unit = {

  }

}

object Tetromino {

  def getTetrominoType(randomNumber : Int, anchor: Point): Tetromino = {
    randomNumber match {
      case 0 => new iTetromino(anchor)
      case 3 => new oTetromino(anchor)
      case _ => new centeredTetromino(randomNumber, anchor)
    }
  }

  def setBlocksAndType(thisT: Tetromino, cell: CellType, body: Vector[Point]):Unit = {
    thisT.blockType = cell
    thisT.bodyBlocks = body
  }

  def getBodyBlocks(points: (Point, Point, Point), anchor: Point) : Vector[Point] = {
    val res = Vector[Point](add(points._1, anchor), anchor, add(points._2, anchor), add(points._3, anchor))
    res
  }



   /* def LBody(anchor:Point): Vector[Point] = {
      val relativePoints =
      val res = getBodyBlocks(relativePoints, anchor)
      res
    }*/

}