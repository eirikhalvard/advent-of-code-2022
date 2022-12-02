object ParseHelpers {
  def splitOnEmpty(list: List[String]): List[List[String]] ={
    list match
      case "" :: rest => splitOnEmpty(rest)
      case Nil => Nil
      case _ =>
        val (group, rest) = list.span(s => s != "")
        group :: splitOnEmpty(rest)
  }
}
