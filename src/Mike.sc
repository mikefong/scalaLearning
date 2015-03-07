object mergesort {
  def msort[T](xs: List[T])(implicit ord: Ordering[T]):List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      /*def merge(xs:List[Int], ys:List[Int]):List[Int] = xs match{
        case Nil => ys
        case x::xs1 =>
          ys match{
            case Nil => xs
            case y::ys1 =>
              if (x<y) x::merge(xs1, ys)
              else y::merge(xs,ys1)
          }
      }*/
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys1) => ys1
        case (xs1, Nil) => xs1
        case (x :: xs1, y :: ys1) if ord.lt(x,y) => x :: merge(xs1, ys)
        case (x :: xs1, y :: ys1)  => y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }
  val x = List(3,2,5,7,4,3)
  msort(x)
  val y = List("world", "hello", "egg")
  msort(y)

  def squareList(xs: List[Int]):List[Int] = xs match{
    case Nil => Nil
    case y::ys => (y*y) :: squareList(ys)
  }

  def squareList2(xs: List[Int]):List[Int] = {
    xs map(x => x*x)
  }

  def pack[T](xs:List[T]):List[List[T]] = xs match{
    case Nil => Nil
    case x::xs =>
      val (first, rest) = xs span( _ == x)
      List(x::first) ::: pack(rest)
  }

  val data =List("a","a","a","b","c","c","a")
  pack(data)

  def encode[T](xs:List[T]) = {
    pack(xs) map (x => (x.head, x.length))
  }
  encode(data)

  def mapFun[T,U](xs:List[T], f:T=>U): List[U] =
    (xs foldRight List[U]())((x,y) => f(x)::y)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((x,y) => y+1)

  lengthFun(data)

  val sum: (Int, Int) => Int = _ + _
  val sumCurried: Int => Int => Int = sum.curried
}