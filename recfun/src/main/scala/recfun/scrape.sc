import scala.annotation.tailrec

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

balance("(if zero? x max (/ 1 x))".toList)

def test(a: Int, b: Int) = (a, b) match {
  case (0, 0) => 1
  case (1,1) => 2
  case (_,_) => 3
}

test(1,1)
List().isEmpty