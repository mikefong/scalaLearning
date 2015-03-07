object Polynomials{
  class Poly(val terms: Map[Int, Double]){
    def this(bindings: (Int, Double)*) = {
      this(bindings.toMap)
    }
    val m = terms.toMap withDefaultValue 0.0
    /*def + (other: Poly): Poly = {
      new Poly((for(k <- m.keySet ++ other.m.keySet)
        yield (k, m.getOrElse(k,0.0) + other.m.getOrElse(k,0.0))
        ).toMap)
    }*/
    def +(other: Poly): Poly = new Poly((m ++ (other.m map adjust)))
    def adjust(v:(Int, Double)): (Int, Double) = {
      val (exp, coeff) = v
      /*m get exp match{
        case Some(coeff1) => (exp, coeff + coeff1)
        case None => (exp, coeff)
      }*/
      (exp, coeff + m(exp))
    }
    def x(other: Poly): Poly = new Poly(
      (other.m foldLeft m)((t,n) =>{
        val (exp,coeff) = n
        t + (exp -> (t(exp) + coeff))
        /*t.updated(exp, t(exp) + coeff)*/
      })
    )
    override def toString = (for ((k,v) <- m.toList.sorted.reverse)
                              yield (v + "x^" + k)).mkString("+")
  }
  val a = new Poly(0->1, 1->2.3)
  val b = new Poly(0->2.3, 2->1)
  a+b
}