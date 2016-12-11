
abstract class Nat {
  def isZero: Boolean
  def pred: Nat
  def succ: Nat = new Succ(this)
  def + (other: Nat): Nat
  def - (other: Nat): Nat
  def toNative: Int
  override def toString: String = toNative.toString
}

object Zero extends Nat {
  def isZero = true
  def pred = throw new NoSuchElementException
  def + (other: Nat) = other
  def - (other: Nat) = if (other.isZero) this else throw new IndexOutOfBoundsException
  def toNative = 0
}

class Succ(n:Nat) extends Nat {
  def isZero = false
  def pred = n
  def + (other: Nat) = n + other.succ
  def - (other: Nat) = if (other.isZero) this else n - other.pred
  def toNative = n.toNative + 1
}
val zero = Zero
val two = zero.succ.succ
val one = zero.succ
val six = two + two + two
six - two

