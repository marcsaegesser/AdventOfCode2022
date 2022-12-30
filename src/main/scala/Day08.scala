package advent

object Day08 {

  def run(): Unit = {
    val grid = readData(dataFile)
    println(s"Day08.part1 = ${part1(grid)}")
    println(s"Day08.part2 = ${part2(grid)}")
  }

  def part1(grid: Grid): Int =
    findVisible(grid).size

  def part2(grid: Grid) = {
    (for {
      r <- 0 until grid(0).size
      c <- 0 until grid(0).size
    } yield scenicScore(grid, r, c)).max
  }

  def scenicScore(grid: Grid, row: Int, col: Int) = {
    def helper(r: Int, c: Int, rStep: Int, cStep: Int, count: Int): Int = {
      if(r < 0 || r >= grid(0).size)        count
      else if(c < 0 || c >= grid(0).size)   count
      else if(grid(r)(c) >= grid(row)(col)) count+1
      else                                  helper(r+rStep, c+cStep, rStep, cStep, count+1)
    }

    List(
      helper(row-1, col, -1, 0, 0),  // up
      helper(row, col+1, 0, 1, 0),   // right
      helper(row+1, col, 1, 0, 0),   // down
      helper(row, col-1, 0, -1, 0)  // left
    ).reduce(_*_)
  }

  def findVisible(grid: Grid) = {
    val info = analyze(grid)

    (for {
      r <- 0 until grid(0).size
      c <- 0 until grid(0).size
      if isVisible(grid, info, r, c)
    } yield (r, c)).toList
  }

  def isVisible(grid: Grid, info: GridInfo, r: Int, c: Int): Boolean = {
    val g = grid(r)(c)
    val left = info.left(r)(c)
    val right = info.right(r)(c)
    val up = info.up(r)(c)
    val down = info.down(r)(c)

    g > left || g > right || g > up || g > down
  }

  def analyze(grid: Grid): GridInfo = {
    def analyzeRow(row: Vector[Int]): Vector[Int] = {
      row.init.foldLeft(-1, List(-1)) { case ((max, accum), i) =>
        if(i > max) (i,   i +: accum)
        else        (max, max +: accum)
      }._2.reverse.toVector
    }

    val xpose = grid.transpose
    GridInfo(
      grid.map(analyzeRow),
      grid.map(r => analyzeRow(r.reverse).reverse),
      xpose.map(analyzeRow).transpose,
      xpose.map(r => analyzeRow(r.reverse).reverse).transpose
    )
  }

  type Grid = Vector[Vector[Int]]

  def printGrid(grid: Grid): String = {
    grid.map(_.mkString("")).mkString("\n")
  }

  case class GridInfo(left: Grid, right: Grid, up: Grid, down: Grid)

  def parseData(lines: List[String]): Grid = {
    def parseLine(line: String): Vector[Int] =
      line.map(_.asDigit).toVector

    lines.map(parseLine).toVector
  }

  def readData(file: String): Grid =
    parseData(io.Source.fromFile(file).getLines().toList)

  val testData = """30373
                   |25512
                   |65332
                   |33549
                   |35390""".stripMargin.linesIterator.toList

  val dataFile = "data/Day08.txt"
}
