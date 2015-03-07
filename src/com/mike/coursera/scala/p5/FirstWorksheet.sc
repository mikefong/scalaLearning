object FirstWorksheet{
  def concat[T](xs: List[T], ys:List[T]):List[T] = xs match{
    case List() => ys
    case z::zs => z :: concat(zs,ys)
  }

  /**
   * too slow, to be improved
   * @param xs
   * @tparam T
   * @return
   */


  def removeAt[T](n:Int, xs: List[T]) = (xs take n) ::: (xs drop n+1)
  def flatten(xs: List[Any]): List[Any] =  xs match{
    case Nil => Nil
    case (y: List[_])::ys => flatten(y) ::: flatten(ys)
    case y::ys => y :: flatten(ys)
  }

  val xx = List(List(1,1,List(3,4)),5,6,List(List(7,8)))


  def reverse[T](xs:List[T]): List[T] = xs match{
    case List() => List()
    case y::ys => reverse(ys) ++ List(y)
  }
  reverse(xx)




}