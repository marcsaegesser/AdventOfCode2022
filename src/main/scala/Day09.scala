package advent

object Day09 {

  def run(): Unit = {
    val moves = readData(dataFile)
    println(s"Day09.part1 = ${part1(moves)}")
    println(s"Day09.part2 = ${part2(moves)}")
  }

  def part1(moves: List[Move]): Int = {
    runMoves(initialState(2), moves).tailPositions.size
  }

  def part2(moves: List[Move]): Int = {
    runMoves(initialState(10), moves).tailPositions.size
  }

  def runMoves(state: State, moves: List[Move]): State = {
    moves match {
      case Nil     => state
      case m :: ms => runMoves(moveN(state, m.direction, m.count), ms)
    }
  }

  def moveN(state: State, dir: Direction, n: Int): State = {
    if(n == 0) state
    else moveN(move(state, dir), dir, n-1)
  }

  def move(state: State, dir: Direction): State = {
    def updateTail(accum: List[Coord], rest: List[Coord]): List[Coord] = {
      rest match {
        case Nil     => accum
        case t :: ts =>
          val a = accum.head
          if(isTouching(a, t)) rest.reverse ++ accum
          else {
            val next =
              (a.x - t.x, a.y - t.y) match {
                case (dx, dy) => Coord(t.x + dx.sign, t.y + dy.sign)
              }
            updateTail(next +: accum, ts)
          }
      }
    }

    val head =
      dir match {
        case "U" => state.rope.head.up
        case "D" => state.rope.head.down
        case "L" => state.rope.head.left
        case "R" => state.rope.head.right
      }

    val rope = updateTail(List(head), state.rope.tail)
    State(rope.reverse, state.tailPositions + rope.head)
  }

  def isTouching(a: Coord, b: Coord): Boolean =
    Math.abs(a.x - b.x) <= 1 && Math.abs(a.y - b.y) <= 1

  case class State(rope: List[Coord], tailPositions: Set[Coord])
  def initialState(n: Int): State = State(List.fill(n)(Coord(0, 0)), Set.empty[Coord])

  case class Coord(x: Int, y: Int) {
    def up    = Coord(x, y+1)
    def down  = Coord(x, y-1)
    def left  = Coord(x-1, y)
    def right = Coord(x+1, y)
  }

  type Direction = String
  case class Move(direction: Direction, count: Int)

  val moveRegex = """([UDLR]) (\d+)""".r
  def parseMove(s: String): Move = {
    val moveRegex(d, c) = s
    Move(d, c.toInt)
  }

  def parseMoves(lines: List[String]): List[Move] =
    lines.map(parseMove)

  def readData(file: String): List[Move] =
    parseMoves(io.Source.fromFile(file).getLines().toList)

  val testData = """R 4
                   |U 4
                   |L 3
                   |D 1
                   |R 4
                   |D 1
                   |L 5
                   |R 2""".stripMargin.linesIterator.toList

  val testData2 = """R 5
                    |U 8
                    |L 8
                    |D 3
                    |R 17
                    |D 10
                    |L 25
                    |U 20""".stripMargin.linesIterator.toList

  val dataFile = "data/Day09.txt"
}

