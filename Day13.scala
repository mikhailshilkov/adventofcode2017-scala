case class Record(index: Int, range: Int)

def parseLayers(text: String) = {
  text.trim()
    .split("\n")
    .map(_.trim().split(": "))
    .map { case Array(i, r) => Record(i.toInt, r.toInt) }
    .toList
}

def hit(startAt: Int)(r: Record) = (startAt + r.index) % (2 * (r.range - 1)) == 0

def totalSeverity(layers: List[Record]) = {
  layers
    .filter(hit(0))
    .map(l => l.index * l.range)
    .sum
}

def findOptimalStart(layers: List[Record]) ={
  Stream.from(0)
    .filter(d => !layers.exists(hit(d)))
    .head
}

val input = """
0: 3
1: 2
4: 4
6: 4
""" // Your input

val layers = parseLayers(input)
println(totalSeverity(layers))
println(findOptimalStart(layers))