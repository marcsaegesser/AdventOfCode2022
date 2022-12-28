package advent

object Day06 {

  def run(): Unit = {
    val data = readData(dataFile)
    println(s"Day06.part1 = ${findStartOfPacket(data)}")
    println(s"Day06.part2 = ${findStartOfMessage(data)}")
  }

  def findStartOfPacket(data: String): Int = {
    data.sliding(4, 1).indexWhere(s => s.toSet.size == s.size) + 4
  }

  def findStartOfMessage(data: String): Int =
    data.sliding(14, 1).indexWhere(s => s.toSet.size == s.size) + 14

  def readData(file: String): String =
    io.Source.fromFile(file).getLines().take(1).mkString

  val testData = """mjqjpqmgbljsphdztnvjfqwrcgsmlb
                   |bvwbjplbgvbhsrlpgdmjqwftvncz
                   |nppdvjthqldpwncqszvftbrmjlhg
                   |nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg
                   |zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw""".stripMargin.linesIterator.toList

  val dataFile = "data/Day06.txt"
}
