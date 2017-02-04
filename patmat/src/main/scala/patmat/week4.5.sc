val l1 = new Cons(12, Nil)
val l2 = l1.prepend(14)
val l3 = Nil.prepend(1)
Nil.prepend(_:Int)

trait List[+T] {
  def isEmpty: Boolean


  def prepend[S >: T](elem: S) = new Cons(elem, this)

  def head: T

  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false

  override def toString = "(" + head + "," + tail
}

object Nil extends List[Nothing] {
  def isEmpty = true

  def head = throw new NoSuchElementException("Nil.head")

  def tail = throw new NoSuchElementException("Nil.tail")

  override def toString = "Nil)"
}
