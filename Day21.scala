implicit def TimesExt(n: Int) = new {
  def times[T](f: T=>T) = { 
    val l = List.fill(n)(f)
    l.foldRight(identity: T=>T){ (x, y) => y.andThen(x) }
  }
}

def buildDict(text: String) = {
  def permutations(key: String) = {
    def rotate(block: List[String]): List[String] = 
      (block.length - 1 to 0 by -1)
        .map(r => (0 to block(0).length - 1).map(i => block(i)(r).toString).mkString(""))
        .toList
  
    def flip(block: List[String]) = {
      val size = block.length - 1
      (0 to size)
        .map(r => (0 to size).map(i => block(r)(size - i).toString).mkString(""))
        .toList
    }
      
    def rotations(block: List[String]) =
      (0 to 3)
        .scanLeft(block) { case (a, _) => rotate(a) }
        .map(_.mkString("/"))
        .toList
  
    val block = key.split("/").toList
    (rotations(block) ::: rotations(flip(block)))
      .distinct
      .toList
  }

  text
    .trim()
    .split("\n")
    .map(l => l.trim.split(" => "))
    .flatMap(ss => permutations(ss(0)).map(p => p -> ss(1)))
    .toMap
}

def expand(dict: Map[String, String])(art: Vector[String]) = {
  def splitBlocks(art: Vector[String]) = {
    val k = if (art.length % 2 == 0) 2 else 3 
    val width = art.length / k - 1
    (0 to width)
    .map(y => {
      (0 to width)
      .map(x => (0 to k - 1).map(i => art(y * k + i).substring(x * k, x * k + k)).to[Vector])
      .to[Vector]
    })
    .toList
  }
  
  def combine(blocks: List[Vector[Vector[String]]]) = {
    def sub(bs: Vector[Vector[String]]) = {
      (0 to bs(0).length - 1)
        .map(row => {
          bs.map(b => b(row)).foldLeft(""){ case (a, s) => a + s }
        })
        .toList
    }
    blocks.flatMap(sub).to[Vector]
  }

  def expandSingle(art: Vector[String]) = {
    val key = art.mkString("/")
    val value = dict(key)
    value.split("/").to[Vector]
  }
  
  val blocks = splitBlocks(art)
  val expandedBlocks = blocks.map(bs => bs.map(expandSingle))
  combine(expandedBlocks)
}

def solve(input: String, count: Int) = {
  val dict = buildDict(input)
  val initial = Vector(".#.", "..#", "###")
  val result = (count times expand(dict))(initial)
  result.map(l => l.count(_ == '#')).sum
}

val input = """

"""

println("d21p1> " + solve(input, 5))
println("d21p2> " + solve(input, 18))