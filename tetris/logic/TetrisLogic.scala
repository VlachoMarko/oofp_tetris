package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.Point.pDown
import tetris.logic.TetrisLogic._
import tetris.logic.Tetromino.getNewTetromino

/** To implement Tetris, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``tetris`` package.
 */
class TetrisLogic(val randomGen: RandomGenerator,
                  val gridDims : Dimensions,
                  val initialBoard: Seq[Seq[CellType]]) {

  var randomNumber: Int = randomGen.randomInt(7)
  var gameOver : Boolean = false

  var board: Board = new Board(getBoardPoints(gridDims), initialBoard.flatten)
  var activeT: Tetromino = getNewTetromino(randomNumber, getAnchor)

  def this(random: RandomGenerator, gridDims: Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))


  def rotateLeft(): Unit = {
    val temps = (activeT.body, activeT.relatives)
    activeT.rotateLeft()
    collisionHandler(temps)
  }

  def rotateRight(): Unit = {
    val temps = (activeT.body, activeT.relatives)
    activeT.rotateRight()
    collisionHandler(temps)
  }


  def moveLeft(): Unit = {
    val temps = (activeT.body, activeT.relatives)
    activeT.body = activeT.moveLeft()
    collisionHandler(temps)
  }

  def moveRight(): Unit = {
    val temps = (activeT.body, activeT.relatives)
    activeT.body = activeT.moveRight()
    collisionHandler(temps)
  }

  def moveDown(): Unit = {
    if (reachedEnd(activeT.body, gridDims.height, board.storedTetrominos)) {clearRows(); storeActive()}
    val temps = (activeT.body, activeT.relatives)

    activeT.body = activeT.moveDown()

    if (outOfBoard(activeT.body, gridDims, board.storedTetrominos)) {
      collisionHandler(temps)
      storeActive()
      spawnTetromino()
    }
    else collisionHandler(temps)
    if (activeT.body.length < 4) {spawnTetromino()}
  }


  def doHardDrop(): Unit = {

    var temps = (activeT.body, activeT.relatives)
    while (!outOfBoard(activeT.body, gridDims, board.storedTetrominos)){
      temps = (activeT.body, activeT.relatives)
      activeT.body = activeT.moveDown()
    }
    collisionHandler(temps)
    clearRows()
    storeActive()
    spawnTetromino()
  }

  def isGameOver: Boolean = gameOver

  def storeActive() : Unit = board.storedTetrominos = board.storedTetrominos :+ activeT

  def getCellType(p: Point): CellType = {
    if (activeT.body.contains(p)) activeT.bodyType
    else getIfStored(p, board.storedTetrominos)
  }

  def getAnchor: Point = {
    val x: Int = if (gridDims.width % 2 == 0) gridDims.width / 2 - 1 else gridDims.width / 2
    Point(x, 1)
  }

  def collisionHandler(tempVals: (Vector[Point], Vector[Point])): Unit = {
    if (!isLegalMove(activeT.body, board, board.storedTetrominos)) {
      activeT.body = tempVals._1
      activeT.relatives = tempVals._2
    }
  }

  def spawnTetromino(): Unit = {
    randomNumber = randomGen.randomInt(7)
    activeT = getNewTetromino(randomNumber, getAnchor)
    if (!activeT.body.forall(notStored(_)(board.storedTetrominos))) gameOver = true
  }

  def clearRows(): Unit = {
    for (y <- (gridDims.height-1) to 0 by -1){
      val row = for (x <- 0 until gridDims.width) yield Point(x, y)

      while (hasAllFrom(board, row)) {
        for (i <- board.storedTetrominos.indices) {
          board.storedTetrominos(i).body = board.storedTetrominos(i).body.filterNot(partOf(_)(row))
        }
        activeT.body = activeT.body.filterNot(partOf(_)(row))
        downAfterClear(row)
      }
    }

  }


  def downAfterClear(row: IndexedSeq[Point]): Unit = {

    val toMoveA = centeredTetromino(Point(50, 50))
    val toMoveB = centeredTetromino(Point(50, 50))
    toMoveA.body = activeT.body.filter(higherThan(_)(row(0).y))
    activeT.body = activeT.body.filterNot(higherThan(_)(row(0).y))


    for (i <- board.storedTetrominos.indices) {
      val tempBlocks = board.storedTetrominos(i).body.filter(higherThan(_)(row(0).y))
      board.storedTetrominos(i).body = board.storedTetrominos(i).body.filterNot(higherThan(_)(row(0).y))
      tempBlocks.foreach(setPointType(_)(board.storedTetrominos(i).bodyType))

      for (j <- tempBlocks.indices) {
        toMoveB.body = toMoveB.body :+ tempBlocks(j)
      }
    }

    toMoveA.body = toMoveA.moveDown()
    toMoveB.body = toMoveB.moveDown()

    loadBackMoved(toMoveA, toMoveB)
    }

  def loadBackMoved(A : Tetromino, B : Tetromino): Unit = {
    for (i <- A.body.indices) {
    activeT.body = activeT.body :+ A.body (i)
    }

    for (j <- B.body.indices) {
        for (i <- board.storedTetrominos.indices) {
          if (B.body (j).celltype == board.storedTetrominos (i).bodyType) {
          board.storedTetrominos (i).body = board.storedTetrominos (i).body :+ B.body (j)
          }
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

  def hasAllFrom(board: Board, testPoints : IndexedSeq[Point]) : Boolean = {
    var tempPoints: Vector[Point] = Vector[Point]()
    for (i <- board.storedTetrominos.indices) {
      for (j <- board.storedTetrominos(i).body.indices) {
        if (!tempPoints.contains(board.storedTetrominos(i).body(j))) tempPoints = tempPoints :+ board.storedTetrominos(i).body(j)
      }
    }
    for (i <- activeT.body.indices ) {
      if (!tempPoints.contains(activeT.body(i))) tempPoints = tempPoints :+ activeT.body(i)
    }

    tempPoints = tempPoints.filter(partOf(_)(testPoints))

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

    def outOfBoard(body: Vector[Point], grid : Dimensions, stored : Seq[Tetromino]): Boolean = {
      if (!body.forall(notAtEnd(_)(grid, stored))) return true
      false
    }

    def notAtEnd(p: Point)(grid : Dimensions, stored : Seq[Tetromino]): Boolean = {
      if (p.y >= grid.height || !notStored(p)(stored)) false
      else true
    }

    def reachedEnd(body: Vector[Point], height: Int, stored: Seq[Tetromino]): Boolean = {
      for (i <- body.indices) {
        if (body(i).y == height - 1 || !notStored(pDown(body(i)))(stored)) return true
      }
      false
    }


    def notStored(p: Point)(stored : Seq[Tetromino]) : Boolean = {
      for (i <- stored.indices) {
        if (stored(i).body.contains(p)) return false
      }
      true
    }

    def getIfStored(p: Point, stored: Seq[Tetromino]): CellType = {
      for (i <- stored.indices) {
        if (stored(i).body.contains(p)) return stored(i).bodyType
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