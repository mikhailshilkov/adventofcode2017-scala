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


def run1(line: String) = {
  val lengths = line.split(",").map(_.toInt).toList
  val end = permutate(lengths)
  end(0) * end(1)
}

def run2(line: String) = {
  val lengths = line.map(_.toInt).toList ++ List(17, 31, 73, 47, 23)
  val lengths64 = (0 to 63).map(_ => lengths).foldLeft(List[Int]()) { (a, e) => a ++ e }
  val sparseHash = permutate(lengths64)
  val denseHash = sparseHash
    .grouped(16)
    .map(l => l.reduceLeft { (a, c) => a ^ c})
  val hex = denseHash
    .map(_.toHexString)
    .map(s => if (s.length == 1) "0" + s else s)
    .mkString("")
  hex
}

val input = "1,2,3" // Your input
println("d10p1>" + run1(input))
println("d10p2>" + run2(input))
