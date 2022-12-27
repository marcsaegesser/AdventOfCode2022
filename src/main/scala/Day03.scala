package advent

object Day03 {

  def run(): Unit = {
    val sacks = readData(dataFile)
    println(s"Day03.part1 = ${part1(sacks)}")
    println(s"Day03.part2 = ${part2(sacks)}")
  }

  def part1(sacks: List[Rucksack]): Int =
    sacks.map(rucksackPriority).sum

  def part2(sacks: List[Rucksack]): Int = {
    sacks
      .grouped(3)
      .map { g =>
        g.map(_.contents).reduce(_.intersect(_)).head
      }.map(priority)
      .sum
  }

  def rucksackPriority(r: Rucksack): Int =
    priority(r.findCommon)

  case class Rucksack(contents: String) {
    val (a, b) = contents.splitAt(contents.size/2)
    def findCommon: Char = a.intersect(b).head
  }

  def priority(c: Char): Int =
    if(c.isLower) c - 'a' + 1
    else          c - 'A' + 27

  def parseData(data: List[String]): List[Rucksack] =
    data.map(Rucksack.apply)

  def readData(file: String): List[Rucksack] =
    parseData(io.Source.fromFile(file).getLines.toList)

  val testData = """vJrwpWtwJgWrhcsFMMfFFhFp
                   |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
                   |PmmdzqPrVvPwwTWBwg
                   |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
                   |ttgJtRGJQctTZtZT
                   |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin.linesIterator.toList

  val dataFile = "data/Day03.txt"
}
