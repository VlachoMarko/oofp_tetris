package tetris.logic

import tetris.logic.Point.{add, centerRotateLeft, centerRotateRight, iRotateLeft, iRotateRight, pDown, pLeft, pRight}
import tetris.logic.Tetromino.{getBodyBlocks, getRelPoints, rotation, setBodyAndType}

abstract class Tetromino {

  var bodyBlocks : Vector[Point]
  var relativePoints : Vector[Point]
  var blockType : CellType = Empty
  var anchor : Point


  def rotateLeft(): Unit
  def rotateRight(): Unit

  def moveDown(): Vector[Point] = {
    this.anchor = pDown(anchor)
    this.bodyBlocks.map(pDown)
  }

  def moveLeft(): Vector[Point] = {
    this.anchor = pLeft(anchor)
    this.bodyBlocks.map(pLeft)
  }

  def moveRight(): Vector[Point] = {
    this.anchor = pRight(anchor)
    this.bodyBlocks.map(pRight)
  }

}

case class centeredTetromino(override var anchor: Point, var randomType: Int = 0) extends Tetromino {
  var bodyBlocks: Vector[Point] = Vector[Point]()
  var relativePoints : Vector[Point] = Vector[Point]()
  setVars()

  override def rotateLeft(): Unit = {
    rotation(this, centerRotateLeft)
  }

  override def rotateRight(): Unit = {
    rotation(this, centerRotateRight)
  }



  def setVars(): Unit = {
    randomType match {
      case 1 => setBodyAndType(this, getBodyBlocks(getRelPoints(JCell), anchor), JCell);
      case 2 => setBodyAndType(this, getBodyBlocks(getRelPoints(LCell), anchor), LCell);
      case 4 => setBodyAndType(this, getBodyBlocks(getRelPoints(SCell), anchor), SCell);
      case 5 => setBodyAndType(this, getBodyBlocks(getRelPoints(TCell), anchor), TCell);
      case 6 => setBodyAndType(this, getBodyBlocks(getRelPoints(ZCell), anchor), ZCell);
      case _ =>
    }
  }


}

case class oTetromino(override var anchor: Point) extends Tetromino {

  var relativePoints: Vector[Point] = Vector[Point](Point(0, -1), Point(1, -1), Point(1, 0))
  var bodyBlocks: Vector[Point] = getBodyBlocks(relativePoints, anchor)
  blockType = OCell

  override def rotateLeft(): Unit = ()
  override def rotateRight(): Unit = ()
}

case class iTetromino(override var anchor: Point) extends Tetromino {

  var relativePoints: Vector[Point] = Vector[Point](Point(-1, 0), Point(1, 0), Point(2, 0), Point(0,0))
  var bodyBlocks: Vector[Point] = getBodyBlocks(relativePoints, anchor)
  blockType = ICell


  override def rotateLeft(): Unit  = {
    relativePoints = relativePoints.map(iRotateLeft)
    bodyBlocks = relativePoints.map(add(_, anchor))

  }

  override def rotateRight(): Unit = {
    relativePoints = relativePoints.map(iRotateRight)
    bodyBlocks = relativePoints.map(add(_, anchor))

  }

}

object Tetromino {

  def getNewTetromino(randomNumber : Int, anchor: Point): Tetromino = {
    randomNumber match {
      case 0 => iTetromino(anchor)
      case 3 => oTetromino(anchor)
      case _ => centeredTetromino(anchor, randomNumber)
    }
  }

  def setBodyAndType(thisT: Tetromino, body: Vector[Point], cell: CellType):Unit = {
    thisT.relativePoints = getRelPoints(cell)
    thisT.bodyBlocks = body
    thisT.blockType = cell
  }

  def getBodyBlocks(points: Vector[Point], anchor: Point) : Vector[Point] = {
    val res = Vector[Point](add(points(0), anchor), anchor, add(points(1), anchor), add(points(2), anchor))
    res
  }

  def rotation(thisT : Tetromino, f: Point => Point): Unit = {

    thisT.relativePoints = thisT.relativePoints.map(f)
    val tempPoints = thisT.relativePoints.map(add(_, thisT.anchor))

    thisT.bodyBlocks = thisT.bodyBlocks.filter(notAnchor(thisT.anchor))
    thisT.bodyBlocks = tempPoints :+ thisT.anchor

  }

  def notAnchor (p: Point) (anchor: Point): Boolean = p != anchor

  def getRelPoints(cell: CellType): Vector[Point] = {
    cell match {
      case JCell => Vector[Point](Point(-1, -1), Point(-1, 0), Point(1, 0))
      case LCell => Vector[Point](Point(-1, 0), Point(1, 0), Point(1, -1))
      case SCell => Vector[Point](Point(-1, 0), Point(0, -1), Point(1, -1))
      case TCell => Vector[Point](Point(-1, 0), Point(0, -1), Point(1, 0))
      case ZCell => Vector[Point](Point(-1, -1), Point(0, -1), Point(1, 0))
    }
  }



}