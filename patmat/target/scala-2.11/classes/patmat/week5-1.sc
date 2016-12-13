val l1 = 4 :: 2 :: 3 :: 1 :: Nil

isort(l1)

def isort(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case Nil => List(x)
  case y::ys => if (x<y) x::xs else y::insert(x,ys)
}