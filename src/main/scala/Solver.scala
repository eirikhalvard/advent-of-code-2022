import scala.io.Source

trait Solver[ParseType, ResultType] {
  val testString: String
  val filename: String

  def parse(lines: List[String]): ParseType

  def solve(parsed: ParseType): ResultType

  final def runTest(): ResultType =
    solve(parse(testString.linesIterator.toList))

  final def runFile(): ResultType = {
    val filepath: String = s"./input/$filename"
    val source = Source.fromFile(filepath)
    val input = source.getLines.toList
    val res = solve(parse(input))
    source.close()
    res
  }
}

