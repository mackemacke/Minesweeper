package minesweeper

/** Definierar matrisklassen. */
case class Matrix[T](data: Vector[Vector[T]]):
  /** Kontrollerar att alla rader i matrisen har samma längd. Jämför varje efterföljande rad med den första. */
  require(data.forall(row => row.length == data(0).length))

  /** Matrisen representerad som en tupel av (antal rader, antal kolumner). */
  val dim: (Int, Int) = (data.length, data(0).length)

  /** Åtkomst av ett element i en specifik row, col. */
  def apply(row: Int, col: Int): T = data(row)(col)

  /** Uppdatering av matrisen vid en specifik row, col. */
  def updated(row: Int, col: Int)(value: T): Matrix[T] = 
    Matrix(data.updated(row, data(row).updated(col, value)))

  /** Iteration genom varje index row, col och tillämpning av en funktion. */
  def foreachIndex(f: (Int, Int) => Unit): Unit = 
    for r <- data.indices; c <- data(r).indices do f(r, c)

  /** Överskridning av definitionen för att representera matrisen som en sträng. */
  override def toString = 
    s"""Matrix of dim $dim:\n${ data.map(_.mkString(" ")).mkString("\n") }"""

object Matrix:
  /** Fyller matrisen med en specifierad dimension och ett värde. */
  def fill[T](dim: (Int, Int))(value: T): Matrix[T] =
    Matrix[T](Vector.fill(dim._1, dim._2)(value))