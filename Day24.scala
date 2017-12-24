def parseComponents(text: String) =
  input.trim()
    .split("\n")
    .map(_.trim().split("/").map(_.toInt))
    .map { case Array(a1, a2) => (a1, a2) }
    .to[Set]
    
type Component = (Int, Int)
type Bridge = List[Component]
  
def buildBridges(components: Set[Component]) = {
  def impl(chain: Bridge, remaining: Set[Component], port: Int): List[Bridge] = {
    val possible = remaining
      .filter { case (c1, c2) => c1 == port || c2 == port }
      .toList
    possible match {
      case List() => List(chain)
      case _ => possible.flatMap(c => impl(c :: chain, remaining - c, if (c._1 == port) c._2 else c._1))
    }
  }
  impl(List(), components, 0)
}

def findHeaviestWeight(bridges: List[Bridge]) =
  bridges
    .map(b => b.map(e => e._1 + e._2).sum)
    .max

def findLongestWeight(bridges: List[Bridge]) =
  bridges
    .map(b => (b.length, b.map(e => e._1 + e._2).sum))
    .maxBy(p => p._1 * 1000 + p._2)
    ._2

val input = """
0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10
""" // Your input

val components = parseComponents(input)
val bridges = buildBridges(components)
println("d24p1> " + findHeaviestWeight(bridges))
println("d24p2> " + findLongestWeight(bridges))