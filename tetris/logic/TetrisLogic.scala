package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
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

  var randomNumber : Int = randomGen.randomInt(7)

  var boardPoints : Seq[Point] = getBoardPoints(gridDims)
  println("boardPoints: " + boardPoints)

  var activeTetromino: Tetromino = getNewTetromino(randomNumber, getAnchor)
  println("starter body: " + activeTetromino.bodyBlocks)

  var storedTetrominos: Vector[Tetromino] = Vector[Tetromino]()




  def this(random: RandomGenerator, gridDims : Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))


  def rotateLeft(): Unit = {
    activeTetromino.rotateLeft()
  }

  def rotateRight(): Unit = {
    activeTetromino.rotateRight()
  }


  def moveLeft(): Unit = {
    val tempBody = activeTetromino.bodyBlocks
    activeTetromino.moveLeft()
    if (!isLegalMove) {
      activeTetromino.bodyBlocks = tempBody
      storedTetrominos = storedTetrominos :+ activeTetromino
    }
  }

  def moveRight(): Unit = {
    val tempBody = activeTetromino.bodyBlocks
    activeTetromino.moveRight()
    if (!isLegalMove) {
      activeTetromino.bodyBlocks = tempBody
      storedTetrominos = storedTetrominos :+ activeTetromino
    }
  }

  def spawnTetromino(): Unit = {
    randomNumber  = randomGen.randomInt(7)
    activeTetromino = getNewTetromino(randomNumber, getAnchor)
    println("starter body: " + activeTetromino.bodyBlocks)
  }

  def moveDown(): Unit = {
    val tempBody = activeTetromino.bodyBlocks
    activeTetromino.moveDown()

    if (!isLegalMove) {
      activeTetromino.bodyBlocks = tempBody
      storedTetrominos = storedTetrominos :+ activeTetromino
      spawnTetromino()
    }
  }

  def isLegalMove : Boolean = {
    activeTetromino.bodyBlocks.forall(onBoard(_)(boardPoints)) && activeTetromino.bodyBlocks.forall(noCollision(_)(storedTetrominos))
  }

  // TODO implement me
  def doHardDrop(): Unit = ()

  // TODO implement me
  def isGameOver: Boolean = false

  // TODO Tetromino storage and their types
  def getCellType(p : Point): CellType = {

    if (activeTetromino.bodyBlocks.contains(p)) activeTetromino.blockType
    else getIfStored(storedTetrominos, p)
  }

  def getAnchor: Point = {
    val x : Int = if (gridDims.width % 2 == 0) gridDims.width/2-1 else gridDims.width/2
    println("Anchor: " + x + ", " + 1)
    Point(x,1)
  }


}

object TetrisLogic {

  val FramesPerSecond: Int = 5 // change this to speed up or slow down the game

  val DrawSizeFactor = 1.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller



  def makeEmptyBoard(gridDims : Dimensions): Seq[Seq[CellType]] = {
    val emptyLine = Seq.fill(gridDims.width)(Empty)
    Seq.fill(gridDims.height)(emptyLine)
  }


  def getBoardPoints(gridDims: Dimensions): Seq[Point] = {
    gridDims.allPointsInside
  }

  def onBoard(p: Point) (boardPoints : Seq[Point]) : Boolean = {
    boardPoints.contains(p)
  }

  def noCollision(p: Point)(stored : Seq[Tetromino]) : Boolean = {
    for (i <- stored.indices) {
      if (stored(i).bodyBlocks.contains(p)) return false
    }
    true
  }

  def noInitialCollision(): Boolean = {
    // TODO: Check for each square that ActiveTetromino has, if in initialBoard it has a celltype other than Empty
    true
  }

  def getIfStored(stored: Seq[Tetromino], p: Point): CellType = {
    for (i <- stored.indices) {
      if (stored(i).bodyBlocks.contains(p)) return stored(i).blockType
    }
    Empty
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
  val DefaultDims : Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)


  def apply() = new TetrisLogic(new ScalaRandomGen(),
    DefaultDims,
    makeEmptyBoard(DefaultDims))

}