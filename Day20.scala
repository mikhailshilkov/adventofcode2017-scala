implicit class Regex(sc: StringContext) {
  def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
}

case class Point(x: Int, y: Int, z: Int)
def distance(p: Point) = Math.abs(p.x) + Math.abs(p.y) + Math.abs(p.z)

implicit class PointOps(private val p1: Point) extends AnyVal {
  def + (p2: Point): Point = 
    Point(p1.x + p2.x, p1.y + p2.y, p1.z + p2.z)
}

case class Particle(position: Point, speed: Point, acceleration: Point)

def parse(text: String) = {
  def parseNumbers(s: String) = s match {
      case r"[pva]=<(-?\d+)${px},(-?\d+)${py},(-?\d+)${pz}>" => Point(px.toInt, py.toInt, pz.toInt)
    }
  
  def parseLine(line: String) = {
    val parts = line.trim().split(", ").map(parseNumbers)
    Particle(parts(0), parts(1), parts(2))
  }

  text.trim().split("\n").map(parseLine).toList
}

def tick(particle: Particle) = {
  val speed = particle.speed + particle.acceleration
  val position = particle.position + speed
  Particle(position, speed, particle.acceleration)
}

def eliminate(particles: List[Particle]) = {
  particles
    .groupBy(_.position)
    .filter { case (k, v) => v.length == 1 }
    .flatMap { case (k, v) => v }
    .toList
}

def evolve(particles: List[Particle], ticks: Int) = {
  (1 to ticks)
    .foldLeft(particles){
      case(ps, _) => eliminate(ps.map(tick))
    }
}

val input = """
p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>    
p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>
p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>
p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>
""" // Your input

val particles = parse(input)
val closest = particles.zipWithIndex.minBy(x => (distance(x._1.acceleration), distance(x._1.speed)))
println("d20p1> " + closest._2)

println("d20p2> " + evolve(particles, 100).length)