package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(col: Int, row: Int): Int = (col, row) match {
    case (c, r) if (c > r || c < 0 || r < 0) => 0
    case (c, r) if c == 0 || c == r => 1
    case _ => pascal(col, row - 1) + pascal(col - 1, row - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def isBalance(numOpen: Int, chars: List[Char]): Boolean =
      if (numOpen < 0) false
      else chars match {
        case Nil => numOpen == 0
        case head :: tail => head match {
          case '(' => isBalance(numOpen + 1, tail)
          case ')' => isBalance(numOpen - 1, tail)
          case _ => isBalance(numOpen, tail)
        }
      }

    isBalance(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
}
