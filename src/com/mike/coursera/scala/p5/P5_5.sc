object P5_5{
  def reduceLeft[T](xs: List[T])(op: (T,T) => T) ={
    xs match {
      case Nil => throw new Error("H")
      case y::ys => foldLeft(ys)(y)(op)
    }

  }
  def foldLeft[T, U](xs:List[T])(z: U)(op: (U, T) => U):U = {
    xs match{
      case Nil => z
      case y::ys => foldLeft(ys)(op(z,y))(op)
    }
  }

  val x = List("I ", "am ", "good ", "Yeah!")
  reduceLeft(x)(_ + _)
  foldLeft(x)("Hey! ")(_ + _)

  def reduceRight[T](xs:List[T])(op: (T,T) => T):T ={
    xs match {
      case Nil => throw new Error("HH")
      case List(y) => y
      case y::ys => op(y, reduceRight(ys)(op))
    }
  }
  def foldRight[T,U](xs:List[T])(z: U)(op: (T,U) => U):U ={
    xs match {
      case Nil => z
      case y::ys => op(y, foldRight(ys)(z)(op))
    }
  }
  foldRight(x)(" Cool!")(_+_)
  reduceRight(x)(_+_)
}