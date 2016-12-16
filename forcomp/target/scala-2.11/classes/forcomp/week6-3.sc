val a = Map("a" -> 3, "b" -> 5)

val p1 = new Poly((3, 13.0), 0 -> 2.0)
val p2 = new Poly(Map(0 -> 3.0, 1 -> 12.0))
p1 + p2

p1.terms(11)

(1 -> 4.9)._2


class Poly(_terms: Map[Int, Double]) {
  def this(ps: (Int, Double)*) = this(ps.toMap)

  val terms = _terms withDefaultValue 0.0

  override def toString =
    (for ((exp, coef) <- terms.toList.sorted.reverse)
      yield coef + "x^" + exp) mkString " + "

  def +(other: Poly) = new Poly((other.terms foldLeft terms) (addTerm))

  def addTerm(z: Map[Int, Double], term: (Int, Double)): Map[Int,Double] = {
    val (exp, coef) = term
    z + (exp -> (coef+terms(exp)))
  }

  def adjust(term: (Int, Double)) = term match {
    case (exp, coef) => (exp, coef + terms(exp))
  }
}


