package advent

object Day07 {

  def run():  Unit = {
    val tree = readData(dataFile)
    println(s"Day07.part1 = ${part1(tree)}")
    println(s"Day07.part2 = ${part2(tree)}")
  }

  def part1(tree: Tree): Long = {
    val (size, sizes) = dirSizes(tree)
    sizes.values.filter(_ <= 100000).sum
  }

  def part2(tree: Tree): Long = {
    val (size, sizes) = dirSizes(tree)
    val available = totalSize - size
    val toFree = requiredSize - available

    sizes.values.filter(_ >= toFree).toList.sorted.head
  }

  def dirSizes(tree: Tree): (Long, Map[String, Long]) = {
    import Tree._
    def helper(t: Tree, path: String, sizes: Map[String, Long]): (Long, Map[String, Long]) =
      t match {
      case File(n, sz) => (sz, sizes)
      case Directory(n, c) =>
        val (newSizes, size) =
          c.foldLeft((sizes, 0L)) { case ((s, accum), t) =>
            val (sz, szs) = helper(t, s"$path$n/", s)
            (szs, accum+sz)
          }
        (size, newSizes + (s"$path$n/" -> size))
    }

    helper(tree, "", Map.empty[String, Long])
  }

  def countFiles(tree: Tree): Int = {
    import Tree._
    def helper(t: Tree, accum: Int): Int =
      t match {
        case File(_, _) => accum + 1
        case Directory(_, ts) => ts.foldLeft(accum+1) { case (a, t) => helper(t, a) }
      }

    helper(tree, 0)
  }

  def treeToString(tree: Tree): String = {
    import Tree._
    def helper(tr: Tree, prefix: String, result: String): String =
      tr match {
        case File(n, sz) => result + s"""$prefix$n (file, $sz)\n"""
        case Directory(n, ts) => result + ts.foldLeft(s"$prefix$n/\n") { case (accum, t) => helper(t, prefix+"  ", accum) }
      }

    helper(tree, "", "")
  }

  val totalSize = 70000000L
  val requiredSize = 30000000L

  enum Tree {
    case Directory(name: String, contents: List[Tree])
    case File(name: String, size: Long)
  }

  enum Path {
    case Top
    case Node(name: String, l: List[Tree], p: Path, r: List[Tree])
  }

  enum Command {
    case CD(name: String)
    case LS(files: List[String])
  }

  def evalCommand(cmd: Command, loc: Location): Location = {
    import Tree._
    import Command._

    val dirRegex = """dir (\S+)""".r
    val fileRegex = """(\d+) (\S+)""".r
    def addFiles(files: List[String], loc: Location): Location = {
      def addFile(file: String, l: Location): Location =
        file match {
          case dirRegex(n) => insertRightMove(l, Directory(n, Nil))
          case fileRegex(sz, n) => insertRightMove(l, File(n, sz.toLong))
        }

      files.foldLeft(loc) { case (l, f) => addFile(f, l) }
    }

    cmd match {
      case CD("/") => goDown(loc)
      case CD("..") => goUp(loc)
      case CD(dir) => goDownAtName(loc, dir)
      case LS(files) => addFiles(files, loc)
    }
  }

  def evalCommands(cmds: List[Command]): Option[Tree] = {
    val loc = cmds.foldLeft(Location(Some(Tree.Directory("/", Nil)), Path.Top)) { case (l, c) => evalCommand(c, l) }
    goTop(loc).t
  }

  import Tree._
  import Path._

  /*
   * The following is an implementation of a Tree Zipper based
   * on Huet (https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf).
   * This is almost certainly over engineered for this puzzle, but it was
   * an interesting exploration.
   */

  case class Location(t: Option[Tree], p: Path)

  def goLeft(l: Location): Location =
    l match {
      case Location(None, p)    => l
      case Location(Some(t), p) =>
        p match {
          case Top => throw Exception("left of top")
          case Node(n, l1 :: ls, up, r) => Location(Some(l1), Node(n, ls, up, t :: r))
          case Node(_, Nil, _, _) => throw Exception("left of first")
        }
    }

  def goRight(l: Location): Location =
    l match {
      case Location(None, p)    => l
      case Location(Some(t), p) =>
        p match {
          case Top                      => throw Exception("right of top")
          case Node(n, l, up, r1 :: rs) => Location(Some(r1), Node(n, t :: l, up, rs))
          case _                        => throw Exception("right of last")
        }
    }

  def goLast(l: Location): Location =
    l match {
      case Location(None, p) =>
        p match {
          case Top => throw Exception("last of top")
          case Node(_, _, _, _) => l
        }
      case Location(Some(t), p) =>
        p match {
          case Top => throw Exception("last of top")
          case Node(n, l, up, r) => Location(Some(r.last), Node(n, (r.init.reverse :+ t) ++ l, up, Nil))
        }
    }

  def goToName(l: Location, name: String): Location =
    l match {
      case Location(None, p)    => throw Exception("goto name on empty")
      case Location(Some(t), p) =>
        p match {
          case Top => throw Exception("goto name on top")
          case Node(n, l, up, r) =>
            val all = ((l.reverse :+ t) ++ r).toVector
            val at = all.indexWhere {
              case Directory(n, _) if(n == name) => true
              case File(n, _) if (n == name)  => true
              case _ => false
            }
            val (left, right) = all.splitAt(at)
            Location(Some(right.head), Node(n, left.toList.reverse, up, right.toList.tail))
        }
    }

  def goDownAtName(l: Location, name: String): Location =
    goDown(goToName(l, name))

  def goUp(l: Location): Location =
    l match {
      case Location(None, p) =>
        p match {
          case Top => throw Exception("up on top")
          case Node(n, l, up, r) => Location(Some(Directory(n, Nil)), up)
        }
      case Location(Some(t), p) =>
        p match {
          case Top               => throw Exception("up of top")
          case Node(n, l, up, r) => Location(Some(Directory(n, (l.reverse :+ t) ++ r)), up)
        }
    }

  def goDown(l: Location): Location =
    l match {
      case Location(None, p)    => throw Exception("down of Nil")
      case Location(Some(t), p) =>
        t match {
          case File(_, _)             => throw Exception("down of file")
          case Directory(n, Nil)      => Location(None, Node(n, Nil, p, Nil))
          case Directory(n, t1 :: ts) => Location(Some(t1), Node(n, Nil, p, ts))
          case _                      => throw Exception("down of empty")
        }
    }

  def goTop(l: Location): Location =
    l match { case Location(_, p) =>
      p match {
        case Top => l
        case _   => goTop(goUp(l))
      }
    }

  def insertRight(l: Location, add: Tree): Location =
    l match {
      case Location(None, p)    =>
        p match {
          case Top => throw Exception("insert of top")
          case Node(n, l, up, r) => Location(Some(add), Node(n, l, up, r))
        }
      case Location(Some(t), p) =>
        p match {
          case Top               => throw Exception("insert of top")
          case Node(n, l, up, r) => Location(Some(t), Node(n, l, up, add :: r))
        }
    }

  def insertRightMove(l: Location, add: Tree): Location =
    l match {
      case Location(None, p)    => insertRight(l, add)
      case Location(Some(t), p) => goRight(insertRight(l, add))
    }

  def insertLeft(l: Location, add: Tree): Location =
    l match {
      case Location(None, p) =>
        p match {
          case Top               => throw Exception("insert of top")
          case Node(n, l, up, r) => Location(Some(add), Node(n, l, up, r))
        }
      case Location(Some(t), p) =>
        p match {
          case Top               => throw Exception("insert of top")
          case Node(n, l, up, r) => Location(Some(t), Node(n, add :: l, up, r))
        }
    }

  def insertLeftMove(l: Location, add: Tree): Location =
    goLeft(insertLeft(l, add))

  /*
   * Data reading/parsing
   */
  val cdRegex = """\$ cd (\S+)""".r
  val lsRegex = """\$ ls""".r

  def parseData(lines: List[String]): Tree =
    makeTree(parseCommands(lines))

  def makeTree(cmds: List[Command]): Tree =
    evalCommands(cmds).getOrElse(throw Exception("malformed input"))

  def parseCommand(input: List[String]): (Command, List[String]) =
    input.head match {
      case cdRegex(dir) => (Command.CD(dir), input.tail)
      case lsRegex      => (Command.LS(input.drop(1).takeWhile(!_.startsWith("$"))), input.drop(1).dropWhile(!_.startsWith("$")))
    }


  def parseCommands(lines: List[String]): List[Command] = {
    def helper(cmds: List[Command], remaining: List[String]): List[Command] =
      if(remaining.isEmpty) cmds.reverse
      else {
        val (cmd, rest) = parseCommand(remaining)
        helper(cmd +: cmds, rest)
      }

    helper(List.empty[Command], lines)
  }

  def readData(file: String): Tree =
    makeTree(parseCommands(io.Source.fromFile(file).getLines().toList))

  val testData = """$ cd /
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
                   |7214296 k""".stripMargin.linesIterator.toList

  val dataFile = "data/Day07.txt"
}
