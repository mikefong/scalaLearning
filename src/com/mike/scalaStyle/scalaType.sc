object scalaStyle {
  val xs = List(1,2,3,4,5,6)
  xs map {
    case x if x > 5 => "Good"
    case _ => "Bad"
  }
  trait Service
  /**
   * if we don't specify the return value here
   * it will be Object with Service {...}
   * @return
   */
  def make():Service = new Service{
    def getIt() = "123"
  }
  /**
   * type, mutable collection should be invariant
   * immutable collection should be covariant
   */
  trait Collection[+T] {
    def add[U >: T](other: U): Collection[U]
  }
  /**
   * only use type alias when necessary
   * the following doesn't provide much help
   */
  type IntMaker = () => Int
  /**
   * don't use type alias with subtyping
   * since SocketFactory is used to produce Socket,
   * better to use a type alias
   */
  trait SocketAddress
  trait Socket
  /*trait SocketFactory extends (SocketAddress => Socket)*/
  type SocketFactory = (SocketAddress) => Socket

  /**
   * don't link to many higher-order functions in one single call.
   * Better ti give intermediate name
   */
  val votes = Seq(("scala", 1), ("java", 4), ("scala", 10), ("scala", 1), ("python", 10))
  val votesByLang = votes groupBy { case (lang, _) => lang }

  val sumByLang = votesByLang map { case (lang, counts) =>
    val countsOnly = counts map { case (_, count) => count }
    (lang, countsOnly.sum)
  }
  val orderedVotes = sumByLang.toSeq
  .sortBy { case (_, count) => count }
  .reverse
  //Or even better, warp them in a {} to avoid name collision
  val orderedVotes2 = {
    val votesByLang = {}
  }

  /**
   * Performance
   * do optimization when really necessary, first make sure you
   * are using the right collection/method, and then must PROFILE
   * and focus on the hottest loop
   */

  /**
   * Java Collection
   * scala.collection.JavaConverters
   */
  {
    import scala.collection.JavaConverters._
    val list: java.util.List[Int] = Seq(1,2,3,4) asJava
  }

  /**
   * Call by name
   * should be used with care, designed mainly for the DSLs
   *
   */
  var nn = 0
  def xx():Int = {
    nn = nn+1
    nn
  }
  def callByName(func: => Int): Unit ={
    func
    func
  }
  callByName(30)
  nn

  xs flatMap {x => Some(x, x+1)}

  case class XX[A](aa: A)
  XX((3,4,5))
  def tt(x: (Int, Int))={println("1")}
  //def tt(x:Int, y:Int)={println("2")}
  tt(3,4)
  /**
   * flatmap sometimes could be more concise using for-comprehension
   * Also, flatMap is used with Option quite ofen, as it could
   * collapse chains of Option to 1
   */
  val chars = 'a' to 'z'
  val perms = chars flatMap { a =>
    chars flatMap { b =>
      if (a != b) Seq("%c%c".format(a, b))
      else Seq()
    }
  }

  val perms2 = for {
    a <- chars
    b <- chars
    if a != b
  } yield "%c%c".format(a, b)

  val oa = Some(20)
  val ob = Some(30)
  val on = None
  val tt = for{
    x <- oa
    y <- ob
  } yield (x,y)
  val no = for{
    x <- oa
    y <- on
  }yield (x,y)

  oa map {x =>
    ob map {y =>
      (x,y)
    }
  }

  /**
   * Structural typing, just like duck type.
   * But try not to use them, since the implementation is not optimized
   */
  val aa  = 30
  aa.asInstanceOf[{def haha()}].haha()
}

