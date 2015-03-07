object mergesort {
  def msort(xs: List[Int]):List[Int] = {
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
      def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (Nil, ys1) => ys1
        case (xs1, Nil) => xs1
        case (x :: xs1, y :: ys1) if x < y => x :: merge(xs1, ys)
        case (x :: xs1, y :: ys1) if x > y => y :: merge(xs, ys1)
      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }
  val x = List(3,2,5,7,4,3)
  msort(x)

}