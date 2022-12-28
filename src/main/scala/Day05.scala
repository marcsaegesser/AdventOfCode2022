package advent

object Day05 {

  def run(): Unit = {
    val state = readData(dataFile)
    println(s"Day05.part1 = ${part1(state)}")
    println(s"Day05.part2 = ${part2(state)}")
  }

  def part1(state: State): String = {
    runMoves(state)
      .toList
      .sortBy(_._1)
      .map(_._2.head)
      .mkString
  }

  def part2(state: State): String = {
    runMoves9001(state)
      .toList
      .sortBy(_._1)
      .map(_._2.head)
      .mkString
  }

  def runMoves(state: State): Stacks = {
    state.moves.foldLeft(state.stacks) { (ss, m) =>
      performMove(m, ss)
    }
  }

  def performMove(move: Move, stacks: Stacks): Stacks = {
    val (m, r) = stacks(move.from).splitAt(move.n)
    val to = stacks(move.to)

    stacks
      .updated(move.to, m.reverse ++ to)
      .updated(move.from, r)
  }

  def runMoves9001(state: State): Stacks = {
    state.moves.foldLeft(state.stacks) { (ss, m) =>
      performMove9001(m, ss)
    }
  }

  def performMove9001(move: Move, stacks: Stacks): Stacks = {
    val (m, r) = stacks(move.from).splitAt(move.n)
    val to = stacks(move.to)

    stacks
      .updated(move.to, m ++ to)
      .updated(move.from, r)
  }

  type Stacks = Map[Int, List[Char]]

  case class Move(n: Int, from: Int, to: Int)

  case class State(stacks: Stacks, moves: List[Move])

  def parseStacks(lines: List[String]): Stacks = {
    def parseStack(s: List[Char]): (Int, List[Char]) = {
      val ss = s.init.dropWhile(_.isWhitespace)
      val id = s.last.toString.toInt
      (id, ss)
    }

    val stacks =
      lines
        .transpose
        .grouped(4)
        .flatMap(_.drop(1).take(1))
        .toList

    stacks.map(parseStack).toMap
  }

  val moveRegex = """move (\d+) from (\d+) to (\d+)""".r
  def parseMove(line: String): Move = {
    val moveRegex(n, f, t) = line
    Move(n.toInt, f.toInt, t.toInt)
  }

  def parseData(lines: List[String]): State = {

    val stacksLines = lines.takeWhile(!_.isEmpty)
    val stacks = parseStacks(stacksLines)

    val moveLines = lines.dropWhile(!_.isEmpty).drop(1)
    val moves = moveLines.map(parseMove)

    State(stacks, moves)
  }

  def readData(file: String): State =
    parseData(io.Source.fromFile(file).getLines.toList)

  val testData = """    [D]    
                   |[N] [C]    
                   |[Z] [M] [P]
                   | 1   2   3 
                   |
                   |move 1 from 2 to 1
                   |move 3 from 1 to 3
                   |move 2 from 2 to 1
                   |move 1 from 1 to 2""".stripMargin.linesIterator.toList

  val dataFile = "data/Day05.txt"
}
