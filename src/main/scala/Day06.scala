import scala.annotation.tailrec

object Day06 {

  val testInput: String =
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

  case object Task1 extends Solver[String, Int] {
    override val testString: String = testInput
    override val filename: String = "day06.txt"

    override def parse(lines: List[String]): String =
      lines.head

    override def solve(parsed: String): Int =
      solve(parsed, 0, 4)

    @tailrec
    def solve(protocolPart: String, offset: Int, size: Int): Int =
      if (isStartOfPacket(protocolPart, size))
        offset + size
      else solve(protocolPart.tail, offset + 1, size)

    def isStartOfPacket(protocol: String, size: Int): Boolean =
      protocol.take(size).toSet.size == size
  }

  case object Task2 extends Solver[String, Int] {
    override val testString: String = testInput
    override val filename: String = Task1.filename

    override def parse(lines: List[String]): String = Task1.parse(lines)

    override def solve(parsed: String): Int =
      Task1.solve(parsed, 0, 14)
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
