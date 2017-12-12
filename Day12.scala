type Graph = Map[Int, List[Int]]

def parseGraph(text: String) = {
  def parseEdge(line: String) = line.split(" <-> ") match {
    case Array(n, ns) => n.trim().toInt -> ns.split(", ").map(_.trim().toInt).toList
  }
  text.trim().split("\n").map(parseEdge).toMap
}

def traverse(graph: Graph, start: Int) = {
  def impl(visited: Set[Int], current: Int): Set[Int] = {
    if (visited.contains(current)) visited
    else graph(current).foldLeft(visited + current)(impl)
  }
  
  impl(Set(), start)
}

def countGroups(graph: Graph) = {
  val result = graph.keys.foldLeft((Set[Int](), 0)) { 
    case ((allVisited, count), element) => 
      if (allVisited contains element) (allVisited, count)
      else (allVisited ++ traverse(graph, element), count + 1)
  }
  result._2
}

val input = """
0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5
""" // Your input

val graph = parseGraph(input)
println("d12> " + traverse(graph, 0).size)
println("d12> " + countGroups(graph))