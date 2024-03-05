package minesweeper

object Main:
  val help = """
    Welcome to MINESWEEPER!

    Left click on cell to reveal.
    Hold space + Left click on cell to flag.
    Close window to exit.

    Don't click the bombs!     
    Good luck!
  """

  val dim = (50, 50)
  val w = new LifeWindow(dim._1, dim._2)

  def main(args: Array[String]): Unit =
    println(help)
    w.defaultStart()