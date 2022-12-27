package advent

object Day02 {

  def run(): Unit = {
    val data = readData(dataFile)
    println(s"Day02.part1 = ${part1(data)}")
    println(s"Day02.part2 = ${part2(data)}")
  }

  def part1(data: List[String]): Int =
    playGame(parseData1(data))

  def part2(data: List[String]): Int =
    playGame(parseData2(data))

  enum Move {
    case Rock
    case Paper
    case Scissors
  }

  case class Round(them: Move, you: Move)

  def playGame(rounds: List[Round]): Int =
    rounds.map(playRound).sum

  def playRound(round: Round): Int =
    round.you match {
      case Move.Rock     =>
        round.them match {
          case Move.Rock     => 1 + 3
          case Move.Paper    => 1
          case Move.Scissors => 1 + 6
        }
      case Move.Paper    =>
        round.them match {
          case Move.Rock     => 2 + 6
          case Move.Paper    => 2 + 3
          case Move.Scissors => 2
        }
      case Move.Scissors =>
        round.them match {
          case Move.Rock     => 3
          case Move.Paper    => 3 + 6
          case Move.Scissors => 3 + 3
        }
    }

  def parseMove(s: String): Move =
    s match {
      case "A" => Move.Rock
      case "X" => Move.Rock
      case "B" => Move.Paper
      case "Y" => Move.Paper
      case "C" => Move.Scissors
      case "Z" => Move.Scissors
    }

  def parseOutcome(them: Move, s: String): Move =
    s match {
      case "X" =>    // Lose
        them match {
          case Move.Rock     => Move.Scissors
          case Move.Paper    => Move.Rock
          case Move.Scissors => Move.Paper
        }
      case "Y" =>    // Draw
        them match {
          case Move.Rock     => them
          case Move.Paper    => them
          case Move.Scissors => them
        }
      case "Z" =>    // Win
        them match {
          case Move.Rock     => Move.Paper
          case Move.Paper    => Move.Scissors
          case Move.Scissors => Move.Rock
        }
    }

  val lineRegex = """(\w) (\w)""".r
  def parseLine1(line: String): Round = {
    val lineRegex(t, y) = line

    Round(parseMove(t), parseMove(y))
  }

  def parseLine2(line: String): Round = {
    val lineRegex(t, y) = line

    val them = parseMove(t)
    Round(them, parseOutcome(them, y))
  }

  def parseData1(data: List[String]): List[Round] =
    data.map(parseLine1)

  def parseData2(data: List[String]): List[Round] =
    data.map(parseLine2)

  def readData(file: String): List[String] =
    io.Source.fromFile(file).getLines.toList

  val testData = """A Y
                   |B X
                   |C Z""".stripMargin.linesIterator.toList

  val dataFile = "data/Day02.txt"
}
