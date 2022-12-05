object Day01 {

  case class Elf(food: List[Int], elfNumber: Int)

  case object Task1 extends Solver[List[Elf], Int] {
    override val testString: String = day01Test
    override val filename = "day01.txt"

    override def parse(lines: List[String]): List[Elf] = {
      val groups = ParseHelpers.splitOnEmpty(lines)
      groups.zipWithIndex.map { case (group, index) => Elf(
        food = group.map(_.toInt),
        elfNumber = index + 1
      )}
    }

    override def solve(parsed: List[Elf]): Int = {
      parsed.map (elf => elf.food.sum).max
    }
  }

  case object Task2 extends Solver[List[Elf], Int] {
    override val filename = "day01.txt"
    override val testString: String = day01Test

    override def parse(lines: List[String]): List[Elf] = Task1.parse(lines)

    override def solve(parsed: List[Elf]): Int = {
      val elfSums = parsed.map (elf => elf.food.sum)
      elfSums.sorted.reverse.take(3).sum
    }
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

  val day01Test =
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
