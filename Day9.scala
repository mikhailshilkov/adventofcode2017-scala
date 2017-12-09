import scala.annotation.tailrec

sealed trait State
case class Valid(score: Int, level: Int, garbage: Int) extends State
case class Garbage(score: Int, level: Int, garbage: Int) extends State

def parse(line: String) = {
  @tailrec
  def impl(line: List[Char], state: State): State = (line, state) match {
    case (List(), _) => state
    case ('!' :: _ :: rem, _)           => impl(rem, state)
    case ('<' :: rem, Valid(s, l, g))   => impl(rem, Garbage(s, l, g))
    case ('{' :: rem, Valid(s, l, g))   => impl(rem, Valid(s + l + 1, l + 1, g))
    case ('}' :: rem, Valid(s, l, g))   => impl(rem, Valid(s, l - 1, g))
    case ('>' :: rem, Garbage(s, l, g)) => impl(rem, Valid(s, l, g))
    case (_   :: rem, Garbage(s, l, g)) => impl(rem, Garbage(s, l, g + 1))
    case (_   :: rem, s)                => impl(rem, s)
  }

  impl(line.toList, Valid(0, 0, 0)) match {
    case Valid(s, l, g) => (s, g)
  }
}

val input = "{{<!>},{<!>},{<!>},{<a>}}" // your input (read it from file to avoid issues with quote escaping

val (s, g) = parse(input)
println("d9p1> " + s)
println("d9p2> " + g)