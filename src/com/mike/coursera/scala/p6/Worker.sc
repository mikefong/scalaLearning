object Worker{
  var l = List(1,3,4,List(30,40,List(11)))
  l.flatMap(x => Set(x))
  case class Book(title: String, authors: List[String])
  def m1(books: List[Book]) ={
    for{
      b <- books
      a <- b.authors
      if(a.startsWith("H"))
    }yield b.title
  }
  def m2(books: List[Book]) = {
    books.flatMap(b =>
      b.authors.withFilter(a => a.startsWith("H")).map(y => b.title)
    )
  }
}

