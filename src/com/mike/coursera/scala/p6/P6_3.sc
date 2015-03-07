object P6_3{
  def queens(n: Int): Set[List[Int]] = {
    def placeQueen(k:Int): Set[List[Int]] = {
      k match {
        case 0 => Set(List())
        case _ =>
          for {q <- placeQueen(k - 1)
               col <- 0 until n
               if isSafe(col, q)
          } yield col :: q
      }
    }

    def isSafe(col:Int, queens:List[Int])={
      val queenWithRow = queens.reverse.zipWithIndex
      queenWithRow.forall{
        x =>{
          val (yy, row) = x
          yy != col && Math.abs(row - queens.length) != Math.abs(col - yy)
        }
      }
    }

    placeQueen(n)
  }
  def show(queens: List[Int]) = {
    val len = queens.length
    "\n" + (for(q <- queens.reverse)yield{
      Vector.fill(len)("*").updated(q, "X").mkString(" ")
    }).mkString("\n")
  }
  queens(4) map show mkString "\n"
}