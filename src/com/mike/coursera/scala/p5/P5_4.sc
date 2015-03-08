object P5_4{
  /**
   * several patterns regarding higher-order functions
   * 1 transformaing each element in a certain way;
   * 2 retrie a list of all elements satisfying a criterion
   * 3 combining the elements of a list using an operator
    */

  def scaleList(xs:List[Double], factor:Double)=
    xs map (_*factor)
  def squareList(xs:List[Int]):List[Int] = xs match{
    case Nil => Nil
    case y::ys => y*y :: squareList(ys)
  }
  def squareList2(xs:List[Int]):List[Int] = xs map (x => x*x)


  //filter part
  def posEles(xs:List[Int]):List[Int] = xs match{
    case Nil => Nil
    case y::ys => if( y > 0) y::posEles(ys) else posEles(ys)
  }

  def posEles2(xs:List[Int]):List[Int] = xs filter(_ > 0)

  val nums = List(2,-4,5,7,1)
  nums filter( _ > 0)
  nums filterNot (_ > 0)
  nums partition( _ > 0)
  nums takeWhile( _ > 0)
  nums dropWhile( _ > 0)
  nums span(_ > 0)

  def pack[T](xs:List[T]):List[List[T]] = xs match{
    case Nil => Nil
    case y::ys =>
      val (first, rest) = xs span (_ == y)
      first::pack(rest)
  }
  val data = List("a", "a", "a", "b", "c", "c")
  pack(data)

  def encode[T](xs:List[T]):List[(T,Int)] = {
    val pck = pack(xs)
    pck map (x => (x.head, x.length))
  }
  encode(data)
}