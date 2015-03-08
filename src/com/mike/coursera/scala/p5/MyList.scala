package com.mike.coursera.scala.p5

/**
 * Created by chufang on 15-3-2.
 */
object MyList extends App{
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
  def reverse[T](xs:List[T]): List[T] = xs match{
    case List() => List()
    case y::ys => reverse(ys) ++ List(y)
  }

  def removeAt[T](n:Int, xs: List[T]) = (xs take n) ::: (xs drop n+1)
  def flatten(xs: List[Any]): List[Any] =  xs match{
    case Nil => Nil
    case (y:List[_])::ys => flatten(y) ::: flatten(ys)
    case y::ys => y :: flatten(ys)
  }

  val xx = List(List(1,1,List(3,4)),5,6,List(List(7,8)))
  println(flatten(xx))

  def msort(xs: List[Int]):List[Int] = {
    val n = xs.length/2
    if (n == 0) xs
    else{
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
      def merge(xs:List[Int], ys:List[Int]):List[Int] = (xs,ys) match{
        case (Nil, ys1) => ys1
        case (xs1, Nil) => xs1
        case (x::xs1, y::ys1) if x < y =>x::merge(xs1, ys)
        case (x::xs1, y::ys1) if x > y => y::merge(xs, ys1)
      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }
}
