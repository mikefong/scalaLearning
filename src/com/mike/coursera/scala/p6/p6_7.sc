object x{
  var words=List("HELLO", "MIKE")
  val mnem=Map(
    2->"ABC", 3->"DEF", 4->"GHI", 5->"JKL",
    6->"MNO", 7->"PQRS", 8-> "TUV", 9->"WXYZ"
  )
  val charCode:Map[Char, Char]={
    for((k,v) <- mnem; c<-v)
      yield (c,k.toString.charAt(0))
  }
  def wordCode(word:String):String = {
    word.toUpperCase() map charCode //map is also function
  }
  wordCode("Java")

  val wordsForNumber:Map[String, Seq[String]]={
    words groupBy (wordCode)
  }

  def encode(number:String): Set[List[String]] = {
    wordsForNumber(number)
  }
}