object Day08 {

  val testInput: String =
    """30373
      |25512
      |65332
      |33549
      |35390
      |""".stripMargin

  case class Tree(height: Int, row: Int, column: Int)

  case object Task1 extends Solver[List[List[Tree]], Int] {
    override val testString: String = testInput
    override val filename: String = "day08.txt"

    override def parse(lines: List[String]): List[List[Tree]] =
      lines.zipWithIndex.map { case (rowString, row) =>
        rowString.toList.zipWithIndex.map { case (numChar, column) =>
          Tree(numChar.toString.toInt, row, column)
        }
      }

    override def solve(parsed: List[List[Tree]]): Int = {
      val left = parsed
      val right = parsed.map(_.reverse)
      val top = parsed.transpose
      val bottom = parsed.reverse.transpose
      val visibleTrees =
        visibleTreesFromView(left) union
          visibleTreesFromView(right) union
          visibleTreesFromView(top) union
          visibleTreesFromView(bottom)
      visibleTrees.size
    }

    def visibleTreesFromView(treeView: List[List[Day08.Tree]]): Set[Tree] =
      treeView.flatMap(treeRow => increasingPrefix(treeRow)).toSet

    def increasingPrefix(treeRow: List[Tree]): List[Tree] =
      treeRow match
        case first :: rest =>
          first :: treeRow.zip(rest).takeWhile { case (t1, t2) => t1.height < t2.height }.map(_._2)
          treeRow
            .foldLeft((List(first), first)) { case ((acc, prevTree), currTree) =>
              if (prevTree.height < currTree.height)
                (currTree :: acc, currTree)
              else (acc, prevTree)
            }
            ._1
        case Nil => List.empty
  }

  case object Task2 extends Solver[List[List[Tree]], Int] {
    override val testString: String = testInput
    override val filename: String = Task1.filename

    override def parse(lines: List[String]): List[List[Tree]] = Task1.parse(lines)

    override def solve(parsed: List[List[Tree]]): Int =
      ???

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
