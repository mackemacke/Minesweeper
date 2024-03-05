package minesweeper

import introprog.PixelWindow
import introprog.PixelWindow.Event
import java.awt.Color as JColor

object LifeWindow:
  val EventMaxWait = 10
  var blockSize = 20
  var defaultColor = JColor.gray
  var revealColor = JColor(156, 156, 156)
  var flagColor = JColor.red
  var gridColor = JColor.lightGray
  var bombColor = JColor.black
  var borderColor = JColor(105, 105, 105)
  var menuColor = JColor.lightGray
  var bombsAround1 = JColor.yellow
  var bombsAround2 = JColor.orange
  var bombsAround3 = JColor(132, 0, 0)
  var bombsAround4 = JColor(77, 13, 9)
  var pointsColor = JColor.red
  var pointsGrid = JColor.black
  val title: String = "Minesweeper"

class LifeWindow(rows: Int, cols: Int):
  import LifeWindow.*

  var life = Life.empty(rows, cols)
  val window: PixelWindow = PixelWindow(cols * blockSize, rows * blockSize, title)
  var quit = false
  var play = false
  var keyPressed = false
  var points = 0
  var timer = 0
  var tick = 0
  var clickedCells: Set[(Int, Int)] = Set()

  def drawCell(row: Int, col: Int, color: JColor): Unit =
    window.fill(col * blockSize + 1, row * blockSize + 1, blockSize - 1 * 2, blockSize - 1 * 2, color)

  def drawGrid(): Unit =
    window.fill(0, 0, cols * blockSize, rows * blockSize, menuColor)
    window.fill(0, 0, cols * blockSize, blockSize, borderColor)
    window.fill(0, 0, blockSize, rows * blockSize, borderColor)
    window.fill(980, 0, blockSize, rows * blockSize, borderColor)
    window.fill(0, 140, cols * blockSize, rows * blockSize, borderColor)
    window.fill(0, 160, cols * blockSize, rows * blockSize, gridColor)
    life.cells.foreachIndex((row, col) =>
      drawCell(row + 8, col, defaultColor)
    )
    displayPoints()

  def pointGrid(): Unit =
    window.fill(40, 40, blockSize * 6, 80, pointsGrid)

  def timerGrid(): Unit =
    window.fill(840, 40, blockSize * 6, 80, pointsGrid)

  def drawText(text: String, x: Int, y: Int, color: JColor = pointsColor, size: Int = 50, style: Int = java.awt.Font.BOLD, fontName: String = java.awt.Font.MONOSPACED): Unit =
    window.drawText(text, x, y, color, size, style, fontName)

  def displayPoints(): Unit =
    pointGrid()
    val centerX = 60
    val centerY = 50
    drawText(s"$points", centerX, centerY)

  def displayTimer(): Unit =
    timerGrid()
    val centerX = 850
    val centerY = 50
    drawText(s"$timer", centerX, centerY)

  def gameOverText(): Unit =
    val centerX = 360
    val centerY = 50
    drawText("GAME OVER", centerX, centerY)


  def revealAdjacentGrayBoxes(row: Int, col: Int, counter: Int = 0): Unit =
  if counter < 15 then
    for {
      r <- -1 to 1
      c <- -1 to 1
      if r != 0 || c != 0
      newRow = row + r
      newCol = col + c
      if newRow >= 0 && newRow > rows - 43 && newCol >= 0 && newCol < cols
    } {
      val currentPos = (newRow, newCol)
      if (!clickedCells.contains(currentPos) && life(newRow, newCol) == 0) {
        clickedCells += currentPos
        points += 1
        drawCell(newRow, newCol, revealColor)
        revealAdjacentGrayBoxes(newRow, newCol, counter + 1)
      }
    }

  def handleClick(pos: (Int, Int)): Unit =
    val cellPos = (pos._2 / blockSize, pos._1 / blockSize)
    if pos._2 / blockSize >= 8 && pos._1 / blockSize >= 0 then
      if !clickedCells.contains(cellPos) then
        clickedCells += cellPos
        points += 1

        if keyPressed then
          drawCell(cellPos._1, cellPos._2, flagColor)
          points += -1
          clickedCells -= cellPos
        else if (life(cellPos._1, cellPos._2) == 1)
          println("bomb!")
          drawCell(cellPos._1, cellPos._2, bombColor)
          gameOverText()
          quit = true
        else if (life(cellPos._1, cellPos._2) == 2)
          drawCell(cellPos._1, cellPos._2, bombsAround1)
        else if (life(cellPos._1, cellPos._2) == 4)
          drawCell(cellPos._1, cellPos._2, bombsAround2)
        else if (life(cellPos._1, cellPos._2) == 6)
          drawCell(cellPos._1, cellPos._2, bombsAround3)
        else if (life(cellPos._1, cellPos._2) == 8)
          drawCell(cellPos._1, cellPos._2, bombsAround4)
        else
          drawCell(cellPos._1, cellPos._2, revealColor)
          revealAdjacentGrayBoxes(cellPos._1, cellPos._2)

  def handleKey(key: String): Unit =
    println(s"$key")
    key match
      case " " => keyPressed = true
      case _   =>

  def spawnBombs(): Unit =
    life = Life.random(rows, cols)

  def addNeighbours(): Unit =
    life.cells.foreachIndex((row, col) =>
      life = life.addValue(row, col)
    )

  def loopUntilQuit(): Unit = while !quit do
    window.awaitEvent(EventMaxWait)
    tick += 1
    if tick % 100 == 0 then
      timer += 1
    displayTimer()
    while window.lastEventType != PixelWindow.Event.Undefined do
      displayPoints()
      window.lastEventType match
        case Event.KeyPressed   => handleKey(window.lastKey)
        case Event.KeyReleased  => keyPressed = false
        case Event.MousePressed => handleClick(window.lastMousePos)
        case Event.WindowClosed => quit = true
        case _                  =>
      window.awaitEvent(EventMaxWait)

  def defaultStart(): Unit =
    spawnBombs()
    addNeighbours()
    drawGrid()
    loopUntilQuit()
