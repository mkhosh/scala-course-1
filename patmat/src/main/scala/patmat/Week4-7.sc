trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval * e2.eval
  }

  override def toString: String = this match {
    case Number(n) => n.toString
    case Sum(e1, e2) => e1.toString + " + " + e2.toString
    case Prod(e1, e2) => (e1,e2) match {
      case (Sum(_,_),_) => "("+e1.toString+")"+" * "+ e2.toString
      case (_,Sum(_,_)) => e1.toString + " * "+ "("+e2.toString+")"
      case _ => e1.toString + " * " + e2.toString
    }
  }
}

case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Prod(e1: Expr, e2: Expr) extends Expr

val e1 = Number(3)
val e2 = Number(4)
val e3 = Number(5)
val e4 = Sum(Prod(e1, e2), e3)
e4.eval
val e5 = Prod(Sum(e1, e2), e3)
e5.eval
val e6 = Prod(e1, Sum(e2, e3))
e6.eval

