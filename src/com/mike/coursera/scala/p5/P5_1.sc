object Play{
  val x = List(30,40,50)
  x.head
  x.tail
  x.last
  x.init
  x.length
  x take 2
  x drop 2
  val y = List(1,2,3)
  x ++ y
  x ::: y
  x.reverse
  x.updated(1,20)
  x(1)
  x indexOf 50
  x contains(40)

  def last[T](xs: List[T]): T = xs match{
    case List() => throw new Error("last of empty list")
    case List(x) => x
    case y::ys => last(ys)
  }
  last(x)

  def init[T](xs: List[T]): List[T] = xs match{
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y::ys =>y::init(ys)
  }
  init(x)

  def concat[T](xs:List[T], ys:List[T]) = (xs foldRight ys){_ :: _}
  concat(x,y)

  def concat2[T](xs:List[T], ys:List[T]):List[T] = xs match{
    case List() => ys
    case z::zs => z::concat2(zs, ys)
  }
  concat2(x,y)

  def reverse[T](xs: List[T]):List[T] = xs match{
    case List() => List()
    case y::ys => reverse(ys) ::: List(y)
  }
  reverse(x)

  def removeAt[T](n:Int, xs:List[Int]):List[Int] = (n, xs) match{
    case (_, Nil) => Nil
    case (0, y::ys) => ys
    case (t, _) if t < 0 => throw new Error("negative number")
    case (t, y::ys) => y::removeAt(t-1, ys)

  }

  def removeAt2[T](n:Int, xs:List[T]):List[T] = (xs take n) ::: (xs drop n+1)


  removeAt(3,List(1,2,3,4,5,6))
  removeAt(100, List())
  //removeAt(-1, List(20))

  removeAt2(3,List(1,2,3,4,5))
  /*def reverse2[T](xs:List[T]):List[T] = (xs foldRight List[T]()){ _ :: _ }*/

}