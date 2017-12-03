val input = 1024 // Your input

type Position = (Int, Int)

def nextPosition(x: Int, y: Int) = {
  val ax = Math.abs(x);
  val ay = Math.abs(y);

  if (y > 0 && ax <= ay)      (x + 1, y)
  else if (x < 0 && ay <= ax) (x,     y + 1)
  else if (y < 0 && ax <= ay) (x - 1, y)
  else                        (x,     y - 1)
}

// Part 1

def mahattanDistance(search:Int) = {
  
  def impl (search: Int, pos: Position, value: Int): Int = {
    val (x, y) = pos
    if (value == search) Math.abs(x) + Math.abs(y)
    else {
      impl(search, nextPosition(x, y), value + 1)
    }
  }
  
  impl(search, (1, 0), 2)
}

println(mahattanDistance(input))

//// Mathy faster version
// def mahattanDistance(number: Int) = {
//   val sqrt = Math.sqrt(number).ceil.toInt
//   val circle = if (sqrt % 2 == 1) sqrt else sqrt + 1 
//   val stepsInside = (circle - 1) / 2
//   val stepsSideway = List.range(0, 4)
//     .map(side => circle * circle - circle / 2 - side * (circle - 1))
//     .map(center => Math.abs(number - center))
//     .min
//   stepsInside + stepsSideway
// }

// Part 2

type Spiral = Map[Position, Int]

def findSpiralSum(search: Int) = {
  
  def neighborSum(x: Int, y: Int, traversed: Spiral) = {
    (for (xx <- List(x-1, x, x+1);
          yy <- List(y-1, y, y+1))
     yield traversed(xx, yy))
    .sum
  }
  
  def impl (search: Int, pos: Position, traversed: Spiral): Int = {
    val (x, y) = pos
    val value = neighborSum(x, y, traversed)
    if (value > search) value
    else {
      val newTraversed = traversed + ((x, y) -> value)
      impl(search, nextPosition(x, y), newTraversed)
    }
  }
  
  val initial = Map((0, 0) -> 1).withDefaultValue(0)
  impl(search, (1, 0), initial)
}

println(findSpiralSum(input))