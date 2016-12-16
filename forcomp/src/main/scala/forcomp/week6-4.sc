val aMap = Map(1 -> "abc", 2 -> "def")

val c = for ((d, cs) <- aMap; c <- cs) yield (c, d)

val words = List(1, 2, 4, 5, 4, 2, 1)

words groupBy (_ == 2)

val f = new Function1[Int,Int] {
  def apply(x:Int) = x * x
}

f(3)

words(4)