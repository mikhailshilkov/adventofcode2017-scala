import scala.annotation.tailrec

case class Point(x: Int, y: Int)

def mazify(text: String) = 
  text.split("\n").filter(!_.trim().isEmpty).to[Vector]

implicit class Maze(val value: Vector[String]) extends AnyVal {
  def at(point: Point) =
    if (point.x >= 0 && point.y >= 0
      && point.y < value.length && point.x < value(point.y).length)
      value(point.y)(point.x)
    else ' '
    
  def findStart() = Point(value(0).indexOf('|'), 0)
  
  def neighbors(point: Point) =
    List((-1, 0), (1, 0), (0, -1), (0, 1))
    .map { case (dx, dy) => Point(point.x + dx, point.y + dy) }
}

def solve(maze: Maze) = {
  val navs = List('-', '|', '+')

  @tailrec
  def navigate(maze: Maze, point: Point, previous: Point, breadcrumbs: String, steps: Int): (String, Int) = {
    val symbol = maze.at(point)
    val next = 
      if (symbol != '+') 
        Point(2 * point.x - previous.x, 2 * point.y - previous.y)
      else
        maze.neighbors(point)
        .filter(_ != previous)
        .filter(maze.at(_) != ' ')
        .head

    val newBreadcrumbs = if (!navs.contains(symbol)) breadcrumbs + symbol else breadcrumbs
    
    if (maze.at(next) == ' ') (newBreadcrumbs, steps)
    else navigate(maze, next, point, newBreadcrumbs, steps + 1)
  }
  
  val start = maze.findStart()
  navigate(maze, start, Point(start.x, -1), "", 1)
}

val input = """
     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+
""" // Your input

val maze = mazify(input)
val solution = solve(maze)
println("d19p1> " + solution._1)
println("d19p2> " + solution._2)