package tetris.logic

import tetris.logic.Point.{add, centerRotateLeft, centerRotateRight}
import tetris.logic.Tetromino.{getBodyBlocks, setBlocksAndType}

abstract class Tetromino {

  var bodyBlocks : Vector[Point]
  var blockType : CellType = Empty
  var anchor : Point

  final def JBody: Vector[Point] = Vector[Point](Point(-1,-1), Point(-1,0), Point(1,0))
  final def LBody: Vector[Point] = Vector[Point](Point(-1,0), Point(1,0), Point(1,-1))
  final def SBody: Vector[Point] = Vector[Point](Point(-1,0), Point(0,-1), Point(1,-1))
  final def TBody: Vector[Point] = Vector[Point](Point(-1,0), Point(0,-1), Point(1,0))
  final def ZBody: Vector[Point] = Vector[Point](Point(-1,-1), Point(0,-1), Point(1,0))


  def rotateLeft(): Unit
  def rotateRight(): Unit

}

class centeredTetromino(randomType: Int, override var anchor: Point) extends Tetromino {
  var bodyBlocks: Vector[Point] = Vector[Point]()
  private var relativePoints : Vector[Point] = Vector[Point]()
  setVars()


  override def rotateLeft(): Unit = {
    rotation(centerRotateLeft)
    println("we left: " + bodyBlocks)
  }


  override def rotateRight(): Unit = {
    rotation(centerRotateRight)
    println("we right: " + bodyBlocks)
  }

  def rotation(f: Point => Point): Unit = {

    relativePoints = relativePoints.map(f)
    val tempPoints = relativePoints

    bodyBlocks = bodyBlocks.filter(notAnchor)
    bodyBlocks = tempPoints.map(add(_, anchor))
    bodyBlocks = bodyBlocks :+ anchor
  }

  def notAnchor (p: Point): Boolean = p != anchor
  def setVars(): Unit = {
    randomType match {
      case 1 => setBlocksAndType(this, JCell, getBodyBlocks(super.JBody, anchor)); relativePoints = JBody
      case 2 => setBlocksAndType(this, LCell, getBodyBlocks(super.LBody, anchor)); relativePoints = LBody
      case 4 => setBlocksAndType(this, SCell, getBodyBlocks(super.SBody, anchor)); relativePoints = SBody
      case 5 => setBlocksAndType(this, TCell, getBodyBlocks(super.TBody, anchor)); relativePoints = TBody
      case 6 => setBlocksAndType(this, ZCell, getBodyBlocks(super.ZBody, anchor)); relativePoints = ZBody
    }
  }


}

class oTetromino(override var anchor: Point) extends Tetromino {

  private var relativePoints: Vector[Point] = Vector[Point](Point(0, -1), Point(1, -1), Point(1, 0))
  var bodyBlocks: Vector[Point] = getBodyBlocks(relativePoints, anchor)
  blockType = OCell

  def rotateLeft(): Unit = ()
  def rotateRight(): Unit = ()
}

class iTetromino(override var anchor: Point) extends Tetromino {

  private var relativePoints: Vector[Point] = Vector[Point](Point(-1, 0), Point(1, 0), Point(2, 0))

  var bodyBlocks: Vector[Point] = getBodyBlocks(relativePoints, anchor)
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

  def getBodyBlocks(points: Vector[Point], anchor: Point) : Vector[Point] = {
    val res = Vector[Point](add(points(0), anchor), anchor, add(points(1), anchor), add(points(2), anchor))
    res
  }



   /* def LBody(anchor:Point): Vector[Point] = {
      val relativePoints =
      val res = getBodyBlocks(relativePoints, anchor)
      res
    }*/

}