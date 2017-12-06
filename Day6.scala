import scala.annotation.tailrec

def parseNumbers(text: String) = {
  text
    .trim()
    .split("\\s+")
    .map(_.trim().toInt)
    .toList
}

def countCycles(text: String) = {
  
  def redistribute(banks:List[Int], index: Int) = {
    val count = banks(index)
    val toEach = count / banks.length
    val remaining = count % banks.length
    val getsRemainder = 
       if (index + remaining < banks.length) (i: Int) => i > index && i <= index + remaining
       else (i: Int) => i > index || i <= (index + remaining - banks.length)
    banks
      .zipWithIndex
      .map { 
        case (e, i) => toEach + (
          if (i == index) 0 
          else if (getsRemainder(i)) e + 1
          else e)
      }
  }
  
  @tailrec
  def impl(banks: List[Int], step: Int, history: Map[List[Int], Int]): (Int, Int) = {
    val previous = history(banks)
    if (previous > 0) (step, step - previous)
    else {
      val newHistory = history + (banks -> step)
      val max = banks.zipWithIndex.maxBy(_._1)._2
      val newBanks = redistribute(banks, max)
      impl(newBanks, step + 1, newHistory)
    }
  }

  val initial = parseNumbers(text)
  impl(initial, 0, Map().withDefaultValue(0))
}

val input = "0 2 7 0" // Your input

val result = countCycles(input)
println("d6p1> " + result._1)
println("d6p2> " + result._2)