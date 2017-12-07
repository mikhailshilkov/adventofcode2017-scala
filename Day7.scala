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
sealed trait Node
case class Branch(name: String, value: Int, children: List[Node]) extends Node
case class Leaf(name: String, value: Int) extends Node

def buildTree(lines: List[Line]) = {
  
  def buildNode(name: String): List[Node] = {
    lines
      .filter(_.name == name)
      .map(line => {
        if (line.children.isEmpty) Leaf(line.name, line.value)
        else Branch(line.name, line.value, line.children.flatMap(buildNode))
      })
  }
  
  val allChildren = lines.flatMap(_.children)
  val root = lines.filterNot(i => allChildren contains i.name)(0)
  Branch(root.name, root.value, root.children.flatMap(buildNode))
}

// Finding unbalanced disk
sealed trait SearchResult
case class FoundUnbalanced(desiredWeight: Int) extends SearchResult
case class NodeInfo(totalWeight: Int, ownWeight: Int) extends SearchResult

def findUnbalanced(node: Node): SearchResult = node match {
  case Leaf(_, value) => NodeInfo(value, value)
  case Branch(_, value, children) => {
    val subs = children.map(findUnbalanced)
    val foundItem = subs.find { 
      case FoundUnbalanced(v) => true
      case _ => false
    }
    foundItem match {
      case Some(diff) => diff
      case None => {
        val values = subs.collect { case NodeInfo(t, o) => NodeInfo(t, o) }
        val groups = values.groupBy(_.totalWeight).values
        // If all sums are equal, return total weight
        if (groups.size == 1) NodeInfo(values.map(_.totalWeight).sum + value, value)
        else {
          // If sums are different, we found the victim, so calculate desired weight and return
          val min = groups.minBy(_.size).head
          val max = groups.maxBy(_.size).head
          FoundUnbalanced(min.ownWeight + max.totalWeight - min.totalWeight)
        }
      }
    }
  }
}

def findUnbalancedDisk(text: String) = {
  val lines = parseLines(text)
  val tree = buildTree(lines)
  findUnbalanced(tree) match { case FoundUnbalanced(d) => d }
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