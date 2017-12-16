def gen(seed: Long, multiplier: Long, predicate: Long => Boolean) = 
  Iterator.iterate(seed)(a => (a * multiplier)  % 2147483647)
    .filter(predicate)

def generatorsMatch(count: Int, g1: Iterator[Long], g2: Iterator[Long]) = {
  g1.zip(g2)
    .take(count)
    .filter(v => (v._1 & 65535) == (v._2 & 65535))
    .length
}

val input = (65, 8921) // Your input

val g1a = gen(input._1, 16807, _ => true)
val g1b = gen(input._2, 48271, _ => true)
println("d15p1> " + generatorsMatch(40000000, g1a, g1b))

val g2a = gen(input._1, 16807, _ % 4 == 0)
val g2b = gen(input._2, 48271, _ % 8 == 0)
println("d15p2> " + generatorsMatch(5000000, g2a, g2b))