import scala.annotation.tailrec

val l1 = new Cons(1, new Cons(4, new Nil))
val l2 = singlton(12)
val l3 = new Cons(14,l1)
nth(-1,l3)


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

def singlton[T](elem: T) = new Cons(elem, new Nil)

def nth[T](n: Int, xs: List[T]): T =
  if (xs.isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) xs.head
  else nth(n - 1, xs.tail)
