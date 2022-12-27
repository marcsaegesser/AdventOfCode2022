package advent

object Day04 {

  def run(): Unit = {
    val pairs = readData(dataFile)
    println(s"Day04.part1 = ${part1(pairs)}")
    println(s"Day04.part2 = ${part2(pairs)}")
  }

  def part1(pairs: List[Pair]): Int = {
    pairs.filter(isFullOverlap).size
  }

  def part2(pairs: List[Pair]): Int =
    pairs.filter(isOverlap).size

  def isOverlap(pair: Pair): Boolean =
    !pair.a.intersect(pair.b).isEmpty

  def isFullOverlap(pair: Pair): Boolean = {
    val int = pair.a.intersect(pair.b)
    int == pair.a || int == pair.b
  }

  case class Pair(a: Set[Int], b: Set[Int])

  val pairRegex = """(\d+)-(\d+),(\d+)-(\d+)""".r

  def parsePair(line: String) = {
    val pairRegex(a, b, c, d) = line
    Pair(a.toInt.to(b.toInt).toSet, c.toInt.to(d.toInt).toSet)
  }

  def parseData(data: List[String]): List[Pair] =
    data.map(parsePair)

  def readData(file: String): List[Pair] =
    parseData(io.Source.fromFile(file).getLines.toList)

  val testData = """2-4,6-8
                   |2-3,4-5
                   |5-7,7-9
                   |2-8,3-7
                   |6-6,4-6
                   |2-6,4-8""".stripMargin.linesIterator.toList

  val dataFile = "data/Day04.txt"
}
