package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._
import tetris.logic.Tetromino.getNewTetromino

import java.awt.GridLayout


/** To implement Tetris, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``tetris`` package.
 */
class TetrisLogic(val randomGen: RandomGenerator,
                  val gridDims : Dimensions,
                  val initialBoard: Seq[Seq[CellType]]) {

  var randomNumber: Int = randomGen.randomInt(7)

  var board: Board = new Board(getBoardPoints(gridDims), initialBoard.flatten)
  var activeTetromino: Tetromino = getNewTetromino(randomNumber, getAnchor)

  def this(random: RandomGenerator, gridDims: Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))


  def rotateLeft(): Unit = {
    val temps = (activeTetromino.bodyBlocks, activeTetromino.relativePoints)
    activeTetromino.rotateLeft()
    collisionHandler(temps)

  }

  def rotateRight(): Unit = {
    val temps = (activeTetromino.bodyBlocks, activeTetromino.relativePoints)
    activeTetromino.rotateRight()
    collisionHandler(temps)
  }


  def moveLeft(): Unit = {
    val temps = (activeTetromino.bodyBlocks, activeTetromino.relativePoints)
    activeTetromino.bodyBlocks = activeTetromino.moveLeft()
    collisionHandler(temps)
  }

  def moveRight(): Unit = {
    val temps = (activeTetromino.bodyBlocks, activeTetromino.relativePoints)
    activeTetromino.bodyBlocks = activeTetromino.moveRight()
    collisionHandler(temps)
  }

  def moveDown(): Unit = {
    if (atEnd(activeTetromino.bodyBlocks, gridDims.height)) clearRows()
    val temps = (activeTetromino.bodyBlocks, activeTetromino.relativePoints)

    activeTetromino.bodyBlocks = activeTetromino.moveDown()

    if (endOfBoard(activeTetromino.bodyBlocks, gridDims, board.storedTetrominos)) {
      collisionHandler(temps)
      board.storedTetrominos = board.storedTetrominos :+ activeTetromino
      spawnTetromino()
    }
    else collisionHandler(temps)
  }


  def doHardDrop(): Unit = {

    while (!endOfBoard(activeTetromino.bodyBlocks, gridDims, board.storedTetrominos)){
      activeTetromino.bodyBlocks = activeTetromino.moveDown()
    }
    val temps = (activeTetromino.bodyBlocks, activeTetromino.relativePoints)
    collisionHandler(temps)
    clearRows()
  }

  // TODO implement me
  def isGameOver: Boolean = false

  def getCellType(p: Point): CellType = {
    if (activeTetromino.bodyBlocks.contains(p)) activeTetromino.blockType
    else getIfStored(p, board.storedTetrominos)
  }

  def getAnchor: Point = {
    val x: Int = if (gridDims.width % 2 == 0) gridDims.width / 2 - 1 else gridDims.width / 2
    println("Anchor: " + x + ", " + 1)
    Point(x, 1)
  }

  def collisionHandler(tempVals: (Vector[Point], Vector[Point])): Unit = {
    if (!isLegalMove(activeTetromino.bodyBlocks, board, board.storedTetrominos)) {
      println("collision!")
      activeTetromino.bodyBlocks = tempVals._1
      activeTetromino.relativePoints = tempVals._2
    }
  }

  def spawnTetromino(): Unit = {
    randomNumber = randomGen.randomInt(7)
    activeTetromino = getNewTetromino(randomNumber, getAnchor)
    println("starter body: " + activeTetromino.bodyBlocks)
  }

  def clearRows(): Unit = {
    for (y <- (gridDims.height-1) to 0 by -1){
      val row = for (x <- 0 until gridDims.width) yield Point(x, y)
      if (hasAllFrom(row)) {

        println("inside")
        for (i <- board.storedTetrominos.indices) {
          board.storedTetrominos(i).bodyBlocks = board.storedTetrominos(i).bodyBlocks.filterNot(partOf(_)(row))

        }
        activeTetromino.bodyBlocks = activeTetromino.bodyBlocks.filterNot(partOf(_)(row))

        downAfterClear(row)

      }
    }
  }


  //TODO: Implement this
  def downAfterClear(row: IndexedSeq[Point]): Unit = {

    val toMoveA = centeredTetromino(Point(50, 50))
    val toMoveB = centeredTetromino(Point(50, 50))
    toMoveA.bodyBlocks = activeTetromino.bodyBlocks.filter(higherThan(_)(row(0).y))
    activeTetromino.bodyBlocks = activeTetromino.bodyBlocks.filterNot(higherThan(_)(row(0).y))

    for (i <- board.storedTetrominos.indices) {
      val tempBlocks = board.storedTetrominos(i).bodyBlocks.filter(higherThan(_)(row(0).y))
      board.storedTetrominos(i).bodyBlocks = board.storedTetrominos(i).bodyBlocks.filterNot(higherThan(_)(row(0).y))
      for (j <- tempBlocks.indices) {
        toMoveB.bodyBlocks = toMoveB.bodyBlocks :+ tempBlocks(j)
        toMoveB.bodyBlocks.foreach(setPointType(_)(board.storedTetrominos(i).blockType))
      }
    }

    toMoveB.bodyBlocks = toMoveB.moveDown()
    toMoveA.bodyBlocks = toMoveA.moveDown()

    for (i <- toMoveA.bodyBlocks.indices) {
      activeTetromino.bodyBlocks = activeTetromino.bodyBlocks :+ toMoveA.bodyBlocks(i)
    }
    for (j <- board.storedTetrominos.indices; k <- toMoveB.bodyBlocks.indices) {
      if (toMoveB.bodyBlocks(k).celltype == board.storedTetrominos(j).blockType) {
        board.storedTetrominos(j).bodyBlocks = board.storedTetrominos(j).bodyBlocks :+ toMoveB.bodyBlocks(j)
      }
    }

  }

  def setPointType(p: Point)(cellType: CellType): Unit = {
    p.celltype = cellType
  }


  def higherThan(p: Point) (y: Int): Boolean = p.y < y


  def partOf[A](a: A)(b: Seq[A]) : Boolean = {
    b.contains(a)
  }

  def hasAllFrom(testPoints : IndexedSeq[Point]) : Boolean = {
    var tempPoints: Vector[Point] = Vector[Point]()
    for (i <- board.storedTetrominos.indices) {
      for (j <- board.storedTetrominos(i).bodyBlocks.indices) {
        tempPoints = tempPoints :+ board.storedTetrominos(i).bodyBlocks(j)
      }
    }

    for (i <- activeTetromino.bodyBlocks.indices) {
      tempPoints = tempPoints :+ activeTetromino.bodyBlocks(i)
    }

    println("tempPoints: " + tempPoints)
    tempPoints = tempPoints.filter(partOf(_)(testPoints))

    println("testPoints: " + testPoints)
    println(tempPoints.length + " / " + testPoints.length)

    if (tempPoints.length == testPoints.length) return true
    false
  }


}

  object TetrisLogic {

    val FramesPerSecond: Int = 5 // change this to speed up or slow down the game

    val DrawSizeFactor = 1.0 // increase this to make the game bigger (for high-res screens)
    // or decrease to make game smaller


    def makeEmptyBoard(gridDims: Dimensions): Seq[Seq[CellType]] = {
      val emptyLine = Seq.fill(gridDims.width)(Empty)
      Seq.fill(gridDims.height)(emptyLine)
    }

    def getBoardPoints(gridDims: Dimensions): Seq[Point] = {
      gridDims.allPointsInside
    }

    def onBoard(p: Point)(boardPoints: Seq[Point]): Boolean = {
      boardPoints.contains(p)
    }

    def endOfBoard(body: Vector[Point], grid : Dimensions, stored : Seq[Tetromino]): Boolean = {
      if (!body.forall(notAtEnd(_)(grid, stored))) return true
      false
    }

    def notAtEnd(p: Point)(grid : Dimensions, stored : Seq[Tetromino]): Boolean = {
      if (p.y >= grid.height || !notStored(p)(stored)) false
      else true
    }

    def atEnd(body: Vector[Point], height: Int): Boolean = {
      for (i <- body.indices) {
        if (body(i).y == height - 1) return true
      }
      false
    }


    def notStored(p: Point)(stored : Seq[Tetromino]) : Boolean = {
      for (i <- stored.indices) {
        if (stored(i).bodyBlocks.contains(p)) return false
      }
      true
    }

    def getIfStored(p: Point, stored: Seq[Tetromino]): CellType = {
      for (i <- stored.indices) {
        if (stored(i).bodyBlocks.contains(p)) return stored(i).blockType
      }
      Empty
    }

    def isLegalMove(body: Seq[Point], board: Board, storedTetrominos: Seq[Tetromino]): Boolean = {
      (body.forall(onBoard(_)(board.boardPoints))
        && body.forall(notStored(_)(storedTetrominos)))
    }





  // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultDims.width and DefaultDims.height


  val DefaultWidth: Int = 10
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  val DefaultDims: Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)


  def apply() = new TetrisLogic(new ScalaRandomGen(),
    DefaultDims,
    makeEmptyBoard(DefaultDims))

}