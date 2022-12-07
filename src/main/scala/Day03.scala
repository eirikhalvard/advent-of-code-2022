object Day03 {

  case class Rucksack(compartment1: List[Item], compartment2: List[Item]) {
    val overlapping: Set[Item] = compartment1.toSet intersect compartment2.toSet
    val itemSet: Set[Item] = compartment1.toSet union compartment2.toSet
  }

  case class Item(char: Char) {
    val priority: Int =
      if (char.isLower) {
        1 + char.toInt - 'a'.toInt
      } else {
        1 + 26 + char.toInt - 'A'.toInt
      }
  }

  val testInput: String =
    """vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw
      |""".stripMargin

  case object Task1 extends Solver[List[Rucksack], Int] {
    override val testString: String = testInput
    override val filename: String = "day03.txt"

    override def parse(lines: List[String]): List[Rucksack] =
      lines.map { line =>
        val (list1, list2) = line.splitAt(line.length / 2)
        Rucksack(list1.toList.map(Item.apply), list2.toList.map(Item.apply))
      }

    override def solve(parsed: List[Rucksack]): Int =
      parsed.map(_.overlapping.head.priority).sum
  }

  case object Task2 extends Solver[List[Rucksack], Int] {
    override val testString: String = testInput
    override val filename: String = "day03.txt"

    override def parse(lines: List[String]): List[Rucksack] = Task1.parse(lines)

    override def solve(parsed: List[Rucksack]): Int =
      parsed
        .grouped(3)
        .toList
        .map(group =>
          group
            .map(_.itemSet)
            .reduce(_ intersect _)
            .head
            .priority
        )
        .sum
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
