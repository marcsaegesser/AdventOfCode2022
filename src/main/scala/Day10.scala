package advent

object Day10 {

  def run(): Unit = {
    val initialCPU = State.initial(readInstructions(dataFile))
    val initialCRT = CRTState.initial(initialCPU)
    println(s"Day10.part1 = ${part1(initialCPU)}")
    println(s"Day10.part1 = \n${part2(initialCRT)}")
  }

  def part1(state: State) = {
    def helper(s: State, accum: List[Int]): List[Int] =
      if(!s.running) accum.reverse
      else           helper(tickN(s, 40), (s.getX * (s.getClock+1)) +: accum)

    helper(tickN(state, 20), List.empty[Int]).sum
  }

  def part2(crtState: CRTState): String = {
    val result = runCRT(crtState)
    showCRT(result.crt)
  }

  def runCRT(state: CRTState): CRTState = {
    if(!state.cpu.running) state
    else                   runCRT(tickCRT(state))
  }

  def tickCRT(state: CRTState): CRTState = {
    val nextCRT =
      if(Math.abs(state.cpu.getX - state.cpu.getClock%40) <= 1) state.crt.updated(state.cpu.getClock, '#')
      else state.crt
    val nextCPU = tick(state.cpu)

    CRTState(nextCPU, nextCRT)
  }

  def tickN(state: State, n: Int): State =
    if(n == 0 || !state.running) state
    else tickN(tick(state), n-1)

  def tick(state: State): State = {
    import Instruction._
    import State._

    state match {
      case Stopped(_, _, _, _) => state
      case StartOfInstruction(x, c, ip, inst) if ip >= inst.size => Stopped(x, c, ip, inst)
      case StartOfInstruction(x, c, ip, inst) =>
        inst(ip) match {
          case addx(v) => InsideInstruction(x, c+1, inst(ip).cycles-1, ip, inst)
          case noop    => StartOfInstruction(x, c+1, ip+1, inst)
        }
      case InsideInstruction(x, c, 1, ip, inst) =>
        inst(ip) match {
          case addx(v) => StartOfInstruction(x+v, c+1, ip+1, inst)
          case noop    => ???   // Error
        }
      case InsideInstruction(x, c, ic, ip, inst) => InsideInstruction(x, c+1, ic-1, ip, inst)
    }
  }

  def showCRT(crt: Vector[Char]): String =
    crt.sliding(40, 40).map(_.mkString("")).mkString("\n")

  case class CRTState(cpu: State, crt: Vector[Char])
  object CRTState {
    def initial(cpu: State) = CRTState(cpu, Vector.fill(280)('.'))
  }

  // This is all pretty lame, but I wanted to explore the new Scala 3 enum/ADT stuff

  enum State {
    case StartOfInstruction(x: Int, clock: Int, ip: Int, instructions: Vector[Instruction])
    case InsideInstruction(x: Int, clock: Int, ic: Int, ip: Int, instructions: Vector[Instruction])
    case Stopped(x: Int, clock: Int, ip: Int, instructions: Vector[Instruction])

    def running: Boolean =
      this match {
        case StartOfInstruction(_, _, _, _)   => true
        case InsideInstruction(_, _, _, _, _) => true
        case Stopped(_, _, _, _)              => false
      }

    def getX: Int =
      this match {
        case StartOfInstruction(x, _, _, _)   => x
        case InsideInstruction(x, _, _, _, _) => x
        case Stopped(x, _, _, _)              => x
      }
    def getClock: Int =
      this match {
        case StartOfInstruction(_, c, _, _)   => c
        case InsideInstruction(_, c, _, _, _) => c
        case Stopped(_, c, _, _)              => c
      }
  }

  object State {
    def initial(instructions: Vector[Instruction]) =
      StartOfInstruction(1, 0, 0, instructions)
  }

  enum Instruction(val cycles: Int) {
    case noop         extends Instruction(1)
    case addx(v: Int) extends Instruction(2)
  }

  val noopRegex = """(noop)""".r
  val addxRegex = """addx (-?\d+)""".r

  def parseInstruction(s: String): Instruction = {
    s match {
      case addxRegex(v) => Instruction.addx(v.toInt)
      case noopRegex(_) => Instruction.noop
    }
  }

  def parseInstructions(lines: List[String]): Vector[Instruction] =
    lines.map(parseInstruction).toVector

  def readInstructions(file: String): Vector[Instruction] =
    parseInstructions(io.Source.fromFile(file).getLines().toList)

  val testData = """addx 15
                   |addx -11
                   |addx 6
                   |addx -3
                   |addx 5
                   |addx -1
                   |addx -8
                   |addx 13
                   |addx 4
                   |noop
                   |addx -1
                   |addx 5
                   |addx -1
                   |addx 5
                   |addx -1
                   |addx 5
                   |addx -1
                   |addx 5
                   |addx -1
                   |addx -35
                   |addx 1
                   |addx 24
                   |addx -19
                   |addx 1
                   |addx 16
                   |addx -11
                   |noop
                   |noop
                   |addx 21
                   |addx -15
                   |noop
                   |noop
                   |addx -3
                   |addx 9
                   |addx 1
                   |addx -3
                   |addx 8
                   |addx 1
                   |addx 5
                   |noop
                   |noop
                   |noop
                   |noop
                   |noop
                   |addx -36
                   |noop
                   |addx 1
                   |addx 7
                   |noop
                   |noop
                   |noop
                   |addx 2
                   |addx 6
                   |noop
                   |noop
                   |noop
                   |noop
                   |noop
                   |addx 1
                   |noop
                   |noop
                   |addx 7
                   |addx 1
                   |noop
                   |addx -13
                   |addx 13
                   |addx 7
                   |noop
                   |addx 1
                   |addx -33
                   |noop
                   |noop
                   |noop
                   |addx 2
                   |noop
                   |noop
                   |noop
                   |addx 8
                   |noop
                   |addx -1
                   |addx 2
                   |addx 1
                   |noop
                   |addx 17
                   |addx -9
                   |addx 1
                   |addx 1
                   |addx -3
                   |addx 11
                   |noop
                   |noop
                   |addx 1
                   |noop
                   |addx 1
                   |noop
                   |noop
                   |addx -13
                   |addx -19
                   |addx 1
                   |addx 3
                   |addx 26
                   |addx -30
                   |addx 12
                   |addx -1
                   |addx 3
                   |addx 1
                   |noop
                   |noop
                   |noop
                   |addx -9
                   |addx 18
                   |addx 1
                   |addx 2
                   |noop
                   |noop
                   |addx 9
                   |noop
                   |noop
                   |noop
                   |addx -1
                   |addx 2
                   |addx -37
                   |addx 1
                   |addx 3
                   |noop
                   |addx 15
                   |addx -21
                   |addx 22
                   |addx -6
                   |addx 1
                   |noop
                   |addx 2
                   |addx 1
                   |noop
                   |addx -10
                   |noop
                   |noop
                   |addx 20
                   |addx 1
                   |addx 2
                   |addx 2
                   |addx -6
                   |addx -11
                   |noop
                   |noop
                   |noop""".stripMargin.linesIterator.toList

  val dataFile = "data/Day10.txt"
}
