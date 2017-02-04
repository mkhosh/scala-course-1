val s1 = Empty
val s2 = s1.incl(4)
val s3 = s2.incl(7)
val s4 = s3.incl(6)
val s5 = s4.incl(3)
val s6 = s5.incl(6)
val s7 = s6 union s3
val s8 = Empty
val s9 = s8.incl(5)
val s10 = s9.incl(4)
val s11 = s10 union s6

abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  def union(other: IntSet) = other

  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def toString = "{" + left + "-" + elem + "-" + right + "}"

  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x) else this

  override def union(other: IntSet) = left union right union other incl elem
}

if (true) 1 else Empty