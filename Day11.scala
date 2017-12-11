case class Coord(x: Int, y: Int)

def step(coord: Coord, direction: String) = direction match {
  case "n"  => Coord(coord.x,     coord.y + 2)
  case "ne" => Coord(coord.x + 1, coord.y + 1)
  case "se" => Coord(coord.x + 1, coord.y - 1)
  case "s"  => Coord(coord.x,     coord.y - 2)
  case "sw" => Coord(coord.x - 1, coord.y - 1)
  case "nw" => Coord(coord.x - 1, coord.y + 1)
}

def distance(coord: Coord) = {
  val dx = Math.abs(coord.x)
  val dy = Math.abs(coord.y)
  dx + (dy - dx) / 2
}

def steps(path: String) =
  path.split(",").scanLeft(Coord(0, 0))(step).map(distance)

val input = "se,sw,se,sw,sw" // Your input
val trajectory = steps(input)
println("d11p1> " + trajectory.last)
println("d11p2> " + trajectory.max)