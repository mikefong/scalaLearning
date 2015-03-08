object Pairs{
  val x = List(1,2,3,4)
  val y = x map(t => List(List(List(t))))
  val n = 7
  def isPrime(x:Int):Boolean = (2 until x) forall( t => x % t != 0)
  for(
    i <- 1 until n;
    j <- 1 until i;
    if isPrime(i+j)
  )yield (i,j)
  isPrime(5)

  def scalarProduct(xs: List[Double], ys: List[Double]):Double ={
    (for(x <- xs zip ys)yield x._1 * x._2).sum
  }
  scalarProduct(List(1,2,3), List(2,3,4))
  val tt = scalarProduct _

}