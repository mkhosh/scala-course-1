//def prod(f: Int => Int)(a: Int, b: Int): Int =
//  if (a > b) 1 else f(a) * prod(f)(a + 1, b)

def mapReduce(combine: (Int, Int) => Int, ini: Int)(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) ini else combine(f(a), mapReduce(combine, ini)(f)(a + 1, b))

def prod = mapReduce((x, y) => x * y, 1) _

def fac(y: Int) = prod(x => x)(1, y)

fac(4)

val tol = 0.0001

def isGood(x: Double, y: Double): Boolean = Math.abs((x - y) / y) < tol
def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isGood(guess, next)) next
    else iterate(next)
  }

  iterate(firstGuess)
}
fixedPoint(x => 1 + x / 2)(1)

def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2

def sqrt(x: Int) = fixedPoint(averageDamp(x / _))(1)

sqrt(2)
math.abs(-3)

val r1 = new Rational(2)
val r2 = new Rational(-5, 7)
val r3 = new Rational(3, 2)
r1 + r1
r1 + r2
r1 - r2 - r3
-r2
r2 < r3
r2.max(r3)
r1.+(r2)

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator should not be zero!")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a
    else gcd(b, a % b)

  private val g = (if (y > 0) 1 else -1) * math.abs(gcd(x, y))

  val numer: Int = x / g

  val denom: Int = y / g

  override def toString: String =
    if (denom == 1) numer.toString
    else numer + "/" + denom

  def +(other: Rational): Rational =
    new Rational(numer * other.denom + denom * other.numer, denom * other.denom)

  def unary_- = new Rational(-numer, denom)

  def -(other: Rational): Rational = this + -other

  def <(other: Rational) = numer * other.denom < denom * other.numer

  def max(other: Rational) = if (this < other) other else this
}

