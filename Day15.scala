def next(v: Long, k: Long) = ((v * k) % 2147483647).toInt

def gen1(seed: Int, multiplier: Int) = 
  Stream.from(0)
    .scanLeft(seed)((a, _) => next(a, multiplier))

def gen2(seed: Int, multiplier: Int, predicate: Int => Boolean) = 
  Stream.from(0)
    .scanLeft(seed)((a, _) => next(a, multiplier))
    .filter(predicate)


def generatorsMatch(count: Int, g1: Stream[Int], g2: Stream[Int]) = {
  g1.zip(g2)
    .take(count)
    .filter { case (v1, v2) => (v1 & 65535) == (v2 & 65535) }
    .length
}

val input = (65, 8921) // Your input

val g1a = gen1(input._1, 16807)
val g1b = gen1(input._2, 48271)
println("d15p1> " + generatorsMatch(40000000, g1a, g1b))

val g2a = gen2(input._1, 16807, _ % 4 == 0)
val g2b = gen2(input._2, 48271, _ % 8 == 0)
println("d15p2> " + generatorsMatch(5000000, g2a, g2b))