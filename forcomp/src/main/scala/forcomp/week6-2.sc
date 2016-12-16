def queens(n: Int) = {
  def placeQueens(k: Int): Set[List[Int]] = {
    if (k == 0) Set(List())
    else for {
      queens <- placeQueens(k - 1)
      col <- 0 until n
      if isSafe(col, queens)
    } yield col :: queens
  }

  def isSafe(col: Int, queens: List[Int]) =
    ((1 to queens.length) zip queens) forall {
      case (diff, c) => col != c && math.abs(col-c) != diff
    }

  placeQueens(n)
}

val qs = queens(4)

def show(queens:List[Int]) ={
  val lines =
    for (col <- queens.reverse)
    yield Vector.fill(4)("* ").updated(col, "X ").mkString
  "\n\n" + (lines mkString "\n")
}

(qs map show)

List(1,2,3).mkString(",")

val v = test(12)

case class test(x:Int)

Option()