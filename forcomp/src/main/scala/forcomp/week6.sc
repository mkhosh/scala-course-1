val a = Array(2, 4, 12)

a(1) = -12

a

1 until 5
1 to 5

def addDot[T](xs: Iterable[T]) = xs flatMap (List(12, _))

addDot("helloj world")

addDot(List(1, 2, 3))

1 to 10 flatMap (x => 1 to 10 map ((x, _)))

val xs = List(1, 2, 3)
val ys = List(3, 2, 1)

xs.zip(ys).map { case (x, y) => x * y }.sum
xs.zip(ys).foldLeft(0)((z, p) => z + p._1 * p._2)
xs.zip(ys).foldLeft(0)({ case (z, (x, y)) => z + x * y })

xs.reduceLeft((x, y) => x + y)

def isPrime(n: Int) = !((2 until n) exists (n % _ == 0))

isPrime(7)

(1 until 10) flatMap (i =>
  (1 until i) map (j => (j, i))) filter { case (x, y) => isPrime(x + y) }

for {
  i <- 1 until 10
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

(for ((x, y) <- xs zip ys) yield x * y).sum

((0 until xs.length) zip xs) forall {
  case (x,y) => x < y
}

