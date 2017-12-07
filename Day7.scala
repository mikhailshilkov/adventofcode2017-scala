// Parsing text
case class Line(name: String, value: Int, children: List[String])

def parseLines(text: String) = {
  
  def parseLine(line: String) = {
    val (nameValue, children) = line.trim().split(" -> ") match {
      case Array(s1, s2) => (s1, s2.split(", ").toList)
      case Array(s) => (s, List())
    }
    nameValue.trim().split(" ") match {
      case Array(s1, s2) => Line(s1, s2.filterNot("()".toSet).toInt, children)
    }
  }
  
  text.trim().split("\\n").map(parseLine).toList
}

// Building the tree from flat Line list
case class Node(name: String, value: Int, children: List[Node])

def buildTree(lines: List[Line]) = {
  
  def buildNode(name: String): List[Node] = {
    lines
      .filter(_.name == name)
      .map(line => Node(line.name, line.value, line.children.flatMap(buildNode)))
  }
  
  val allChildren = lines.flatMap(_.children)
  val root = lines.filterNot(i => allChildren contains i.name)(0)
  Node(root.name, root.value, root.children.flatMap(buildNode))
}

// Finding unbalanced disk
type FoundUnbalanced = Int
case class NodeInfo(totalWeight: Int, node: Node)

def findUnbalanced(node: Node): Either[NodeInfo, FoundUnbalanced] = {
  // If that's a Leaf, just return itself
  if (node.children.isEmpty) Left(NodeInfo(node.value, node))
  else {
    val subs = node.children.map(findUnbalanced)
    (subs.find(_.isRight)) match {
      // If unbalanced already found in children, just propogate up
      case Some(diff) => diff
      case None => {
        val values = subs.collect { case Left(l) => l }
        val groups = values.groupBy(_.totalWeight).values
        // If all sums are equal, return total weight
        if (groups.size == 1) Left(NodeInfo(values.map(_.totalWeight).sum + node.value, node))
        else {
          // If sums are different, we found the victim, so calculate desired weight and return
          val min = groups.minBy(_.size).head
          val max = groups.maxBy(_.size).head
          Right(min.node.value + max.totalWeight - min.totalWeight)
        }
      }
    }
  }
}

def findUnbalancedDisk(text: String) = {
  val lines = parseLines(text)
  val tree = buildTree(lines)
  findUnbalanced(tree) match { case Right(r) => r }
}

// Run it
val input = """
pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)
""" // Your input

println("d7p1> " + buildTree(parseLines(input)).name)
println("d7p2> " + findUnbalancedDisk(input))