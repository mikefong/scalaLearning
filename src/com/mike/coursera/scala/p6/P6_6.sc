object P6_6{
  val x = Map("Mike" -> 30, "Geo" -> 29)
  //x("Mi")
  x.getOrElse("Mi",30)

  val so = Option("HH")
  val no = Option(null)
  for(x <- no) println("HH")
  for(x <- so) println(x)

  val l = Set("apple", "orange", "pear")
  //l.sorted
  l groupBy (_.length)

}