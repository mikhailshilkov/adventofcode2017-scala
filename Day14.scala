// Begin: copied from Day 10
import scala.annotation.tailrec

def step(vector: Vector[Int], position: Int, length: Int): Vector[Int] = {
  val overflowIndex = if (position + length > vector.length) (position + length) % vector.length else 0
  vector
    .zipWithIndex
    .map { case (x, i) =>
      if (position <= i && i < position + length) {
        vector((position + length - (i - position) - 1) % vector.length) 
      }
      else if (i < overflowIndex) {
        vector((position + length - (vector.length + i - position) - 1)  % vector.length)
      }
      else x 
    }
}

def permutate(lengths: List[Int]) = {
  @tailrec
  def impl(lengths: List[Int], vector: Vector[Int], position: Int, skip: Int): Vector[Int] = lengths match {
    case List() => vector
    case l :: rem => {
      val newVector = step(vector, position, l)
      val newPosition = (position + skip + l) % vector.length
      val newSkip = skip + 1
      impl(rem, newVector, newPosition, newSkip)
    }
  }
  
  val initial = (0 to 255).to[Vector]
  impl(lengths, initial, 0, 0)
}
// End: copied from Day 10

def toBinary(i: Int) =
    String.format("%8s", i.toBinaryString).replace(' ', '0')

def binaryHash(line: String) = {
  val lengths = line.map(_.toInt).toList ++ List(17, 31, 73, 47, 23)
  val lengths64 = (0 to 63).map(_ => lengths).foldLeft(List[Int]()) { (a, e) => a ++ e }
  val sparseHash = permutate(lengths64)
  val denseHash = sparseHash
    .grouped(16)
    .map(l => l.reduceLeft { (a, c) => a ^ c})
  val binary = denseHash
    .map(toBinary(_))
    .mkString("")
  binary
}

def grid(line: String) =
  (0 to 127).map(line + "-" + _).map(binaryHash).to[Vector]

case class Point(x: Int, y: Int)
type Points = Set[Point]

def countOnes(lines: Vector[String]) = {
  lines.mkString("").filter(_ == '1').length
}

def countSubgroups(lines: Vector[String]) = {
  val maxy = lines.length - 1
  val maxx = lines(0).length - 1
  
  def findGroup(acc: Points, visited: Points, point: Point): (Points, Points) = {
    if (visited contains point) (acc, visited)
    else {
      val newVisited = visited + point
      if (lines(point.y).charAt(point.x) != '1') (acc, newVisited)
      else {
        val newAcc = acc + point
        List((-1, 0), (1, 0), (0, -1), (0, 1))
          .map { case (dx, dy) => Point(point.x + dx, point.y + dy) }
          .filter(p => p.x >= 0 && p.y >= 0 && p.x <= maxx && p.y <= maxy)
          .foldLeft((newAcc, newVisited)) { case ((a, v), p) => findGroup(a, v, p) }
      }
    }
  }
  
  val (count, visited) =
    (for (y <- 0 to maxy; x <- 0 to maxx) yield Point(x, y))
    .foldLeft((0, Set[Point]())) { 
      case ((count, visited), p) => {
        val (group, newVisited) = findGroup(Set[Point](), visited, p) 
        (group.toList match { case List() => count case _  => count + 1 }, newVisited)
      }
    }
  count
}

val input = "flqrgnkx" // Your input
val gridLines = grid(input)

println("d12p1> " + countOnes(gridLines))
println("d12p2> " + countSubgroups(gridLines))