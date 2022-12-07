object Day07 {

  val testInput: String =
    """$ cd /
      |$ ls
      |dir a
      |14848514 b.txt
      |8504156 c.dat
      |dir d
      |$ cd a
      |$ ls
      |dir e
      |29116 f
      |2557 g
      |62596 h.lst
      |$ cd e
      |$ ls
      |584 i
      |$ cd ..
      |$ cd ..
      |$ cd d
      |$ ls
      |4060174 j
      |8033020 d.log
      |5626152 d.ext
      |7214296 k
      |""".stripMargin

  enum Command:
    case LS(result: List[LSResult])
    case CD(name: String)

  enum LSResult:
    case Dir(name: String)
    case File(name: String, size: Int)

  enum FileTree:
    case File(name: String, size: Int)
    case Dir(name: String, nodes: List[FileTree])

  case object Task1 extends Solver[List[Command], Int] {
    override val testString: String = testInput
    override val filename: String = "day07.txt"

    override def parse(lines: List[String]): List[Command] =
      parseCommands(lines)

    def parseCommands(lines: List[String]): List[Command] = {
      val LSPattern = "\\$ ls".r
      val CDPattern = "\\$ cd (.*)".r
      lines match
        case head :: tail =>
          head match {
            case LSPattern() =>
              val (lsResultStrings, restLines) = tail.span(s => s.head != '$')
              Command.LS(lsResultStrings.map(parseLSResult)) :: parseCommands(restLines)
            case CDPattern(name: String) =>
              Command.CD(name) :: parseCommands(tail)
          }
        case Nil => Nil
    }

    def parseLSResult(resultString: String): LSResult =
      val DirPattern = "dir (.*)".r
      val FilePattern = "(\\d*) (.*)".r
      resultString match {
        case DirPattern(name)        => LSResult.Dir(name)
        case FilePattern(size, name) => LSResult.File(name, size.toInt)
      }

    override def solve(parsed: List[Command]): Int =
      calculateA(makeFileTree(parsed))

    def makeFileTree(commands: List[Command]): FileTree =
      makeFileTreeNode(commands)._1

    def makeFileTreeNode(commands: List[Command]): (FileTree, List[Command]) =
      commands match
        case Command.CD(name) :: Command.LS(result) :: rest =>
          val (childDirs, restCommands) = parseChildDirs(rest)
          val childFiles = makeFileNodes(result)
          (FileTree.Dir(name, childDirs ++ childFiles), restCommands)
        case Nil => throw new IllegalStateException()
        case _   => throw new IllegalStateException()

    def parseChildDirs(commands: List[Command]): (List[FileTree], List[Command]) =
      commands match {
        case Command.CD("..") :: restCommands =>
          (Nil, restCommands)
        case Nil =>
          (Nil, Nil)
        case _ =>
          val (node, commands2) = makeFileTreeNode(commands)
          val (nodes, commands3) = parseChildDirs(commands2)
          (node :: nodes, commands3)
      }

    def makeFileNodes(result: List[LSResult]): List[FileTree] =
      result.flatMap {
        case LSResult.Dir(name)        => None
        case LSResult.File(name, size) => Some(FileTree.File(name, size))
      }

    def calculateA(fileTree: FileTree): Int = fileTree match
      case FileTree.File(name, size) => 0
      case FileTree.Dir(name, nodes) =>
        val s = calculateSum(fileTree)
        val node = if (s <= 100000) s else 0
        node + nodes.map(calculateA).sum

    def calculateSum(fileTree: FileTree): Int = fileTree match {
      case FileTree.File(name, size) => size
      case FileTree.Dir(name, nodes) => nodes.map(calculateSum).sum
    }
  }

  case object Task2 extends Solver[List[Command], Int] {
    override val testString: String = testInput
    override val filename: String = Task1.filename

    override def parse(lines: List[String]): List[Command] = Task1.parse(lines)

    override def solve(parsed: List[Command]): Int =
      calculateB(Task1.makeFileTree(parsed))

    def calculateB(fileTree: FileTree): Int = {
      val totalSpace = 70000000
      val updateSize = 30000000
      val actualSize = Task1.calculateSum(fileTree)
      def findAllCandidates(node: FileTree): List[Int] =
        node match
          case FileTree.File(name, size) => Nil
          case FileTree.Dir(name, nodes) =>
            val nodeSize = Task1.calculateSum(node)
            val unusedSpaceIfDeleted = totalSpace - actualSize + nodeSize
            if (unusedSpaceIfDeleted >= updateSize)
              nodeSize :: nodes.flatMap(findAllCandidates)
            else nodes.flatMap(findAllCandidates)

      findAllCandidates(fileTree).minBy(identity)
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
}
