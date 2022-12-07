object Day04 {

  val testInput: String =
    """2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8
      |""".stripMargin

  case class ElfPair(elf1: Range, elf2: Range)

  case class Range(start: Int, end: Int) {
    def contains(otherRange: Range): Boolean =
      start <= otherRange.start && otherRange.end <= end
    def overlaps(otherRange: Range): Boolean =
      start <= otherRange.end && otherRange.start <= end
  }

  case object Task1 extends Solver[List[ElfPair], Int] {
    override val testString: String = testInput
    override val filename: String = "day04.txt"

    override def parse(lines: List[String]): List[ElfPair] =
      lines.map { line =>
        val ranges = line.split(',').map { rangeString =>
          val rangeItems = rangeString.split('-').map(_.toInt)
          Range(rangeItems(0), rangeItems(1))
        }
        ElfPair(ranges(0), ranges(1))
      }

    override def solve(parsed: List[ElfPair]): Int =
      parsed.count { case ElfPair(elf1, elf2) =>
        elf1.contains(elf2) || elf2.contains(elf1)
      }
  }

  case object Task2 extends Solver[List[ElfPair], Int] {
    override val testString: String = testInput
    override val filename: String = "day04.txt"

    override def parse(lines: List[String]): List[ElfPair] = Task1.parse(lines)

    override def solve(parsed: List[ElfPair]): Int =
      parsed.count { case ElfPair(elf1, elf2) => elf1.overlaps(elf2) }
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
