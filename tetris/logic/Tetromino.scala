package tetris.logic

import tetris.logic.Point.{add, centerRotateLeft, centerRotateRight, iRotateLeft, iRotateRight, pDown, pLeft, pRight}
import tetris.logic.Tetromino.{getBodyBlocks, getRelPoints, rotation, setBodyAndType}

abstract class Tetromino {

  var body : Vector[Point]
  var relatives : Vector[Point]
  var bodyType : CellType = Empty
  var anchor : Point
  def rotateLeft(): Unit
  def rotateRight(): Unit

  def moveDown(): Vector[Point] = {
    this.anchor = pDown(anchor)
    this.body.map(pDown)
  }

  def moveLeft(): Vector[Point] = {
    this.anchor = pLeft(anchor)
    this.body.map(pLeft)
  }

  def moveRight(): Vector[Point] = {
    this.anchor = pRight(anchor)
    this.body.map(pRight)
  }

}

case class centeredTetromino(override var anchor: Point, var randomType: Int = 0) extends Tetromino {
  var body: Vector[Point] = Vector[Point]()
  var relatives : Vector[Point] = Vector[Point]()
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

  var relatives: Vector[Point] = Vector[Point](Point(0, -1), Point(1, -1), Point(1, 0))
  var body: Vector[Point] = getBodyBlocks(relatives, anchor)
  bodyType = OCell

  override def rotateLeft(): Unit = ()
  override def rotateRight(): Unit = ()

}

case class iTetromino(override var anchor: Point) extends Tetromino {

  var relatives: Vector[Point] = Vector[Point](Point(-1, 0), Point(1, 0), Point(2, 0), Point(0,0))
  var body: Vector[Point] = getBodyBlocks(relatives, anchor)
  bodyType = ICell


  override def rotateLeft(): Unit  = {
    relatives = relatives.map(iRotateLeft)
    body = relatives.map(add(_, anchor))

  }

  override def rotateRight(): Unit = {
    relatives = relatives.map(iRotateRight)
    body = relatives.map(add(_, anchor))

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

  def setBodyAndType(thisT: Tetromino, body: Vector[Point], bodytype: CellType):Unit = {
    thisT.relatives = getRelPoints(bodytype)
    thisT.body = body
    thisT.bodyType = bodytype
  }

  def getBodyBlocks(points: Vector[Point], anchor: Point) : Vector[Point] = {
    val res = Vector[Point](add(points(0), anchor), anchor, add(points(1), anchor), add(points(2), anchor))
    res
  }

  def rotation(thisT : Tetromino, f: Point => Point): Unit = {

    thisT.relatives = thisT.relatives.map(f)
    val tempPoints = thisT.relatives.map(add(_, thisT.anchor))

    thisT.body = thisT.body.filter(notAnchor(thisT.anchor))
    thisT.body = tempPoints :+ thisT.anchor

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