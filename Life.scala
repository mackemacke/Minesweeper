package minesweeper

import minesweeper.Matrix.fill
import scala.util.Random.nextFloat

case class Life(cells: Matrix[Int]):

  def apply(row: Int, col: Int): Int =
    if (isValidCell(row, col)) {
      cells(row, col) match {
        case 1 | 2 | 4 | 6 | 8 => cells(row, col)
        case _ => 0
      }
    } else 0

  def updated(row: Int, col: Int, value: Int): Life =
    Life(cells.updated(row, col)(value))

  def addValue(row: Int, col: Int): Life =
    val oldValue = cells(row, col)
    if (oldValue == 1) {
      var tempCells = cells
      for {
        r <- -1 to 1
        c <- -1 to 1
        if !(r == 0 && c == 0) && isValidCell(row + r, col + c)
      } {
        val currentValue = tempCells(row + r, col + c)
        if (currentValue != 1) {
          tempCells = tempCells.updated(row + r, col + c)(currentValue + 2)
        }
      }
      Life(tempCells)
    } else this


  private def isValidCell(row: Int, col: Int): Boolean =
    row >= 0 && row < cells.dim._1 && col >= 0 && col < cells.dim._2

object Life:

  def empty(dim: (Int, Int)): Life =
    Life(Matrix.fill(dim)(0))

  def random(dim: (Int, Int)): Life =
    val probability = 0.13
    var life = empty(dim)

    life.cells.foreachIndex { (r, c) =>
      if (nextFloat() < probability) {
        life = life.updated(r, c, 1)
      }
    }
    life
