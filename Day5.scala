import scala.annotation.tailrec

def parseMultilineNumbers(text: String) = {
  text
    .trim()
    .split("\n")
    .map(_.trim().toInt)
    .to[Vector]
}

def countJumps(text: String, modify: Int => Int) = {
  
  @tailrec
  def impl(index: Int, step: Int, maze: Vector[Int]): Int = {
    if (index < 0 || index >= maze.length) step
    else {
      val jump = maze(index)
      val newMaze = maze.updated(index, modify(jump))
      impl(index + jump, step + 1, newMaze)
    }
  }

  val initial = parseMultilineNumbers(text)
  impl(0, 0, initial)
}

val input = """
0
3
0
1
-3
""" // Your input

println("d5p1> " + countJumps(input, v => v + 1))
println("d5p2> " + countJumps(input, v => if (v >= 3) v - 1 else v + 1))