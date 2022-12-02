import scala.io.Source

object Day01 {

  case class Parsed(elves: List[Elf])

  case class Elf(food: List[Int], elfNumber: Int)

  def testInput: List[String] = getLines(testString)

  def filename: String = "./input/day01task1.txt"

  def useTest(): Int =
    solve(parse((testInput)))


  def useFile(): Int = {
    val source = Source.fromFile(filename)
    val input = source.getLines.toList
    val res = solve(parse((input)))
    source.close()
    res
  }


  def parse(lines: List[String]): Parsed = {
    val groups = splitOnEmpty(lines)
    Parsed(groups.zipWithIndex.map { case (group, index) => Elf(
      food = group.map(_.toInt),
      elfNumber = index + 1
    ) })
  }

  def getLines(str: String): List[String] = str.linesIterator.toList

  def splitOnEmpty(list: List[String]): List[List[String]] ={
    list match
      case "" :: rest => splitOnEmpty(rest)
      case Nil => Nil
      case _ =>
        val (group, rest) = list.span(s => s != "")
        group :: splitOnEmpty(rest)
  }

  def solve(parsed: Parsed): Int = {
    val elfSums = parsed.elves.map ( elf => (elf.food.sum, elf.elfNumber))
    val bestElf = elfSums.maxBy(_._1)
    bestElf._2
  }

  def main(args: Array[String]): Unit = {
    val result = useFile()
    println(result)
  }

  val testString =
    """|1000
       |2000
       |3000
       |
       |4000
       |
       |5000
       |6000
       |
       |7000
       |8000
       |9000
       |
       |10000
       |""".stripMargin
}
