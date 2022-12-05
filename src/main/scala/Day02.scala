
object Day02 {

  case class StrategyGuide1(moves: List[(Shape, Shape)])
  case class StrategyGuide2(moves: List[(Shape, Result)])

  enum Shape:
    case Rock, Paper, Scissor

  def shapeFromChar(char: Char): Shape = char.toUpper match {
    case 'A'| 'X' => Shape.Rock
    case 'B'| 'Y' => Shape.Paper
    case 'C'| 'Z' => Shape.Scissor
  }

  enum Result:
    case Win, Draw, Loose

  def resultFromChar(char: Char): Result = char.toUpper match {
    case 'X' => Result.Loose
    case 'Y' => Result.Draw
    case 'Z' => Result.Win
  }

  def score(opponentShape: Shape, yourShape: Shape): Int = {
    val shapeScore = yourShape match
      case Shape.Rock => 1
      case Shape.Paper => 2
      case Shape.Scissor => 3

    val outcomeScore = (opponentShape, yourShape) match {
      case (Shape.Rock, Shape.Rock) => 3
      case (Shape.Rock, Shape.Paper) => 6
      case (Shape.Rock, Shape.Scissor) => 0
      case (Shape.Paper, Shape.Rock) => 0
      case (Shape.Paper, Shape.Paper) => 3
      case (Shape.Paper, Shape.Scissor) => 6
      case (Shape.Scissor, Shape.Rock) => 6
      case (Shape.Scissor, Shape.Paper) => 0
      case (Shape.Scissor, Shape.Scissor) => 3
    }

    shapeScore + outcomeScore
  }

  val testInput =
    """A Y
      |B X
      |C Z
      |""".stripMargin

  case object Task1 extends Solver[StrategyGuide1, Int] {
    override val filename: String = "day02.txt"

    override def parse(lines: List[String]): StrategyGuide1 =
      StrategyGuide1(lines.map(s => (shapeFromChar(s.charAt(0)), shapeFromChar(s.charAt(2)))))

    override def solve(parsed: StrategyGuide1): Int = parsed.moves.map{ (opp, you) => score(opp, you) }.sum

    override val testString: String = testInput
  }

  case object Task2 extends Solver[StrategyGuide2, Int] {
    override val filename: String = "day02.txt"

    override def parse(lines: List[String]): StrategyGuide2 =
      StrategyGuide2(lines.map(s => (shapeFromChar(s.charAt(0)), resultFromChar(s.charAt(2)))))

    override def solve(parsed: StrategyGuide2): Int =
      parsed.moves.map { case (opponentShape, result) => score(opponentShape, computeShape(opponentShape, result))}.sum

    def computeShape(opponentShape: Shape, result: Result) = (opponentShape, result) match {
      case (Shape.Rock, Result.Win) => Shape.Paper
      case (Shape.Rock, Result.Draw) => Shape.Rock
      case (Shape.Rock, Result.Loose) => Shape.Scissor
      case (Shape.Paper, Result.Win) => Shape.Scissor
      case (Shape.Paper, Result.Draw) => Shape.Paper
      case (Shape.Paper, Result.Loose) => Shape.Rock
      case (Shape.Scissor, Result.Win) => Shape.Rock
      case (Shape.Scissor, Result.Draw) => Shape.Scissor
      case (Shape.Scissor, Result.Loose) => Shape.Paper
    }

    override val testString: String = testInput
  }

  def main(args: Array[String]): Unit = {
    println("Task1 test result:")
    println(Task1.runTest())

    println("Task1 legit result:")
    println(Task1.runFile())

    println("Task2 test result:")
    println(Task2.runTest())

    println("Task2 legit result:")
    println(Task2.runFile())
  }
}
