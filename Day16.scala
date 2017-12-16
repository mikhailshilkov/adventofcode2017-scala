trait Command
case class Spin(size: Int) extends Command
case class SwapByIndex(p1: Int, p2: Int) extends Command
case class SwapByName(p1: Char, p2: Char) extends Command

implicit class Regex(sc: StringContext) {
  def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
}

def parseCommands(text: String) = {
  def parseCommand(line: String) = line match {
    case r"s(\d+)${s}" => Spin(s.toInt)
    case r"x(\d+)${first}\/(\d+)${second}" => SwapByIndex(first.toInt, second.toInt)
    case r"p([a-z])${first}\/([a-z])${second}" => SwapByName(first(0), second(0))
  }
  text.split(",").toList.map(_.trim()).map(parseCommand)
}

def dance(commands: List[Command])(initial: List[Char]) = {
  commands
    .foldLeft(initial) { case (v, e) => e match {
      case Spin(s) => v.drop(v.size - s) ::: v.take(v.size - s)
      case SwapByIndex(p1, p2) => {
        val vector = v.to[Vector]
        v.updated(p1, vector(p2)).updated(p2, vector(p1))
      }
      case SwapByName(p1, p2) => v.map(c => if (c == p1) p2 else if (c == p2) p1 else c)
    }
  }
}

def findNth(commands: List[Command], count: Int) = {
  val initial = ('a' to 'p').toList
  Iterator
    .iterate(initial)(dance(commands))
    .drop(count)
    .next
    .mkString("")
}

def findCycle(commands: List[Command]) = {
  val initial = ('a' to 'p').toList
  Iterator
    .iterate(initial)(dance(commands))
    .zipWithIndex
    .drop(1)
    .dropWhile {case (v, _) => v != initial }
    .next
    ._2
}

val input = "s1,x3/4,pe/b" // Your input
val commands = parseCommands(input)
val cycle = findCycle(commands)
println("d16p1> " + findNth(commands, 1))
println("d16p2> " + findNth(commands, 1000000000 % cycle))