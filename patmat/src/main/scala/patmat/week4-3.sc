object List {
  def apply[T](): List[T] = new Nil[T]
  def apply[T](elem: T): List[T] = new Cons(elem,apply())
  def apply[T](e1:T, e2:T): List[T] = new Cons(e1,apply(e2))
  def apply[T](e1:T, e2:T, e3:T): List[T] = new Cons(e1,apply(e2,e3))
}

List(1)
List(-12,12.0)

trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false

  override def toString = head + "," + tail
}

class Nil[T] extends List[T] {
  def isEmpty = true

  def head = throw new NoSuchElementException("Nil.head")

  def tail = throw new NoSuchElementException("Nil.tail")

  override def toString = ")"
}
