object P5_3{
  /**
   * the compiler will search an implicit definition that
   * 1 is marked as implicit
   * 2 has a type compatible with T
   * 3 is visible at the point of the function call, or is defined in a companion object associated with T
   * @param xs
   * @param ord
   * @tparam T
   * @return
   */
  def msort[T](xs:List[T])(implicit ord: Ordering[T]):List[T] = {
    val n = xs.length/2;
    if(n == 0) xs
    else{
      def merge(xs:List[T], ys:List[T]):List[T] =
        (xs,ys) match{
          case (Nil, _) => ys
          case (_, Nil) => xs
          case (x::xs1, y::ys1) if ord.lt(x , y) =>
            x::merge(xs1,ys)
          case (x::xs1, y::ys1) => y::merge(xs,ys1)
        }
      val(fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  val nums = List(2,-4,5,7,1)
  msort(nums)

  val xs = List(3,4,5,6)
  val List(x,y,z) = xs

  trait XX[T]
  object XX extends XX[Int]
}

