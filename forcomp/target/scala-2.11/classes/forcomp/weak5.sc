def merge(xs: List[Int], ys: List[Int]): List[Int] =
  (xs, ys) match {
    case (Nil,_) => ys
    case (_,Nil) => xs
    case (x::xs1,y::ys1) => if (x<y) x :: merge(xs1,ys) else y :: merge(xs,ys1)
  }

merge(List(2,4),List(1,3))

val l = List(1, 2, 3)
l.size
l.length
l.take(2)
l.drop(2)
l.splitAt(2)

("what is it", 'a', 12, 2.33)

val l2 = List(5, 6)
l ::: l2
l ++ l2

def init[T](xs: List[T]): List[T] = xs match {
  case Nil => throw new Error("init of empty list")
  case List(x) => Nil
  case y :: ys => y :: init(ys)
}

init(l)

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case Nil => ys
  case z :: zs => z :: concat(zs, ys)
}

concat(l, l2)

def reverse[T](xs: List[T]): List[T] = xs match {
  case Nil => Nil
  case x :: xs => reverse(xs) ::: List(x)
}

reverse(l2)

def removeAt[T](n: Int, xs: List[T]): List[T] = xs.take(n) ++ xs.drop(n + 1)

removeAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)

def flatten(xs: List[Any]): List[Any] = xs match {
  case Nil => Nil
  case (y: List[Any]) :: ys => flatten(y) ++ flatten(ys)
  case y:: ys => y :: flatten(ys)
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))


