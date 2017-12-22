import scala.annotation.tailrec

trait Direction
case class Up()    extends Direction
case class Down()  extends Direction
case class Left()  extends Direction
case class Right() extends Direction

def left(direction: Direction) = direction match {
  case Up() => Left()
  case Left() => Down()
  case Down() => Right()
  case Right() => Up()
}

def right(direction: Direction) = direction match {
  case Up() => Right()
  case Left() => Up()
  case Down() => Left()
  case Right() => Down()
}

trait Status
case class Clean()    extends Status
case class Weakened() extends Status
case class Infected() extends Status
case class Flagged()  extends Status

def simpleInfection(status: Status) = status match {
  case Clean()    => Infected()
  case Infected() => Clean()
}

def slowInfection(status: Status) = status match {
  case Clean()    => Weakened()
  case Weakened() => Infected()
  case Infected() => Flagged()
  case Flagged()  => Clean()
}

def nextDirection(status: Status): Direction => Direction = status match {
  case Clean()    => left _
  case Weakened() => identity _
  case Infected() => right _
  case Flagged()  => left _ compose left
}

case class Point(x: Int, y: Int)
case class Grid(map: Map[Point, Status], position: Point, direction: Direction)

def parseGrid(text: String) = {
  val lines = input.trim().split("\n").map(_.trim())
  val map =
    (for (y <- 0 to lines.length - 1;
          x <- 0 to lines(y).length - 1
         if lines(y)(x) == '#')
      yield Point(x, y) -> Infected())
      .toMap
      .withDefaultValue(Clean())
  val center = Point(lines(0).length / 2, lines.length / 2)
  Grid(map, center, Up())
}

def countInfections(text: String, infection: Status => Status, steps: Int) = {
  def impl(grid: Grid, acc: Int) = {
    val pos = grid.position
    val newDirection = nextDirection(grid.map(grid.position))(grid.direction)
    val newPosition = newDirection match {
      case Up()    => Point(pos.x, pos.y - 1)
      case Left()  => Point(pos.x - 1, pos.y)
      case Down()  => Point(pos.x, pos.y + 1)
      case Right() => Point(pos.x + 1, pos.y)
    }
    val newStatus = infection(grid.map(pos))
    val makesInfected = newStatus match { case Infected() => true case _ => false }
    val newMap = grid.map + (pos -> newStatus)
    val newGrid = Grid(newMap, newPosition, newDirection)
    (newGrid, if (makesInfected) acc + 1 else acc)
  }
  
  (1 to steps)
    .foldLeft((parseGrid(input), 0)) {
      case ((grid, acc), _) => impl(grid, acc)
    }
    ._2
}

val input = """
..#
#..
...
""" // Your input

println(countInfections(input, simpleInfection, 10000))
println(countInfections(input, slowInfection, 10000000))