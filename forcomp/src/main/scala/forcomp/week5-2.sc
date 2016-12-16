import  math.Ordered
def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.size / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs,ys) match {
      case (Nil,_) => ys
      case (_,Nil) => xs
      case (x::xRest,y::yRest) =>
        if (ord.lt(x,y)) x::merge(xRest,ys)
        else y::merge(xs,yRest)
    }
    val (fst, snd) = xs.splitAt(n)
    merge(msort(fst),msort(snd))
  }
}

msort(List(3,2,4))
msort(List("gb","abc","zdeg"))

def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => y*y :: squareList(ys)
  }

def squareList2(xs: List[Int]): List[Int] =
  xs map (x => x*x)

val cs = List("a", "a", "a", "b", "c", "c", "a")

cs.span(_=="a")

pack(cs)

List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => xs.takeWhile(_==x) :: pack(xs.dropWhile(_==x))
}

encode(List("a", "a", "a", "b", "c", "c", "a"))

List(("a", 3), ("b", 1), ("c", 2), ("a", 1))

def encode[T](xs:List[T]) = pack(xs).map(x => (x.head,x.size))


val f = (List(1,2,3) foldLeft List(4,5))_
f((x,y)=>y::x)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( f(_) :: _ )

mapFun(List(1,2,3),(x:Int)=>x*2)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (x,y) => y+1 )

lengthFun(List(1,2,3))
