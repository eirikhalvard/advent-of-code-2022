object Day05 {

  val testInput: String =
    """
      |    [D]    .
      |[N] [C]    .
      |[Z] [M] [P].
      | 1   2   3 .
      |
      |move 1 from 2 to 1
      |move 3 from 1 to 3
      |move 2 from 2 to 1
      |move 1 from 1 to 2
      |""".stripMargin

  case class Stacks(stack: Array[Stack])

  case class Stack(crates: List[Crate]) {
    def pop(n: Int): (List[Crate], Stack) = (crates.take(n), Stack(crates.drop(n)))
    def push(newCrates: List[Crate]): Stack = Stack(newCrates ++ crates)
  }

  case class Crate(char: Char)

  case class Instruction(quantity: Int, from: Int, to: Int) {
    val fromIndex: Int = from - 1
    val toIndex: Int = to - 1
  }

  def applyInstruction(numToMove: Int, fromIndex: Int, toIndex: Int)(stacks: Stacks): Stacks = {
    val (moveCrates, fromStack) = stacks.stack(fromIndex).pop(numToMove)
    val toStack = stacks.stack(toIndex).push(moveCrates)
    Stacks(
      stacks.stack
        .updated(fromIndex, fromStack)
        .updated(toIndex, toStack)
    )
  }
  def executeInstructionV1(stacks: Stacks, instruction: Instruction): Stacks =
    Iterator
      .iterate(stacks)(applyInstruction(1, instruction.fromIndex, instruction.toIndex))
      .drop(instruction.quantity)
      .next

  def executeInstructionV2(stacks: Stacks, instruction: Instruction): Stacks =
    applyInstruction(instruction.quantity, instruction.fromIndex, instruction.toIndex)(stacks)

  case object Task1 extends Solver[(Stacks, List[Instruction]), String] {
    override val testString: String = testInput
    override val filename: String = "day05.txt"

    override def parse(lines: List[String]): (Stacks, List[Instruction]) = {
      val split = ParseHelpers.splitOnEmpty(lines)
      (parseStacks(split(0)), parseInstructions(split(1)))
    }

    def parseStacks(stackLines: List[String]): Stacks = {
      val onlyStacks = stackLines.init
        .map(_.dropRight(1)) // drop manually added dots (.)
      val stacks = onlyStacks
        .map(_.grouped(4).toList)
        .transpose
        .map(_.flatMap(parseCrate))
        .map(Stack.apply)
        .toArray
      Stacks(stacks)
    }

    def parseCrate(str: String): Option[Crate] =
      str(1) match {
        case ' ' => None
        case c   => Some(Crate(c))
      }

    def parseInstructions(instructionLines: List[String]): List[Instruction] =
      instructionLines.map { instructionString =>
        val Pattern = "move (\\d*) from (\\d*) to (\\d*)".r
        instructionString match
          case Pattern(quantity, from, to) =>
            Instruction(quantity.toInt, from.toInt, to.toInt)
      }

    override def solve(parsed: (Stacks, List[Instruction])): String =
      val (stacks, instructions) = parsed
      instructions
        .foldLeft(stacks)(executeInstructionV1)
        .stack
        .flatMap(_.crates.headOption)
        .map(_.char)
        .mkString
  }

  case object Task2 extends Solver[(Stacks, List[Instruction]), String] {
    override val testString: String = testInput
    override val filename: String = Task1.filename

    override def parse(lines: List[String]): (Stacks, List[Instruction]) = Task1.parse(lines)

    override def solve(parsed: (Stacks, List[Instruction])): String =
      val (stacks, instructions) = parsed
      instructions
        .foldLeft(stacks)(executeInstructionV2)
        .stack
        .flatMap(_.crates.headOption)
        .map(_.char)
        .mkString
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
