package advent

object Day01 {

  def run(): Unit = {
    val data = readData(dataFile)
    println(s"Day01.part1 = ${part1(data)}")
    println(s"Day01.part2 = ${part2(data)}")
  }

  def part1(data: List[Int]): Int =
    data.max

  def part2(data: List[Int]): Int =
    data.sorted.reverse.take(3).sum

  def parseData(data: List[String]): List[Int] = {
    def helper(accum: List[Int], sum: Int, remaining: List[String]): List[Int] =
      remaining match {
        case Nil          => (sum +: accum).reverse
        case h :: "" :: t => helper((sum + h.toInt) +: accum, 0, t)
        case h :: t       => helper(accum, sum + h.toInt, t)
      }

    helper(List.empty[Int], 0, data)
  }

  def readData(file: String): List[Int] =
    parseData(io.Source.fromFile(file).getLines.toList)


  val testData = """1000
                   |2000
                   |3000
                   |
                   |4000
                   |
                   |5000
                   |6000
                   |
                   |7000
                   |8000
                   |9000
                   |
                   |10000""".stripMargin.linesIterator.toList

  val dataFile = "data/Day01.txt"
}


