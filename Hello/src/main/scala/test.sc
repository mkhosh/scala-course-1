import scala.annotation.tailrec

def factorial(n: Int) = {
  @tailrec
  def factIter(curN: Int, res: Int): Int =
    if (curN > 1) factIter(curN - 1, res * curN)
    else res

  factIter(n, 1)
}

factorial(5)

def fact2(n: Int): Int = if (n <= 1) 1 else n * fact2(n - 1)

fact2(4)

