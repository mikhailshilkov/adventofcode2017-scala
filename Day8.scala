case class Instruction(
  commandArgument: String, 
  command: Int => Int, 
  conditionArgument: String,
  condition: Int => Boolean)

def parseInstructions(text: String) = {
  def parseInstruction(line: String) = line.trim().split(" if ") match {
    case Array(commandText, conditionText) => {
      val (commandArgument, command) = commandText.split("\\s+") match {
        case Array(ca, "inc", by) => (ca, (x: Int) => x + by.toInt)
        case Array(ca, "dec", by) => (ca, (x: Int) => x - by.toInt)
      }
      val (conditionArgument, condition) = conditionText.split("\\s+") match {
        case Array(ca, ">", cmp) => (ca, (x: Int) => x > cmp.toInt)
        case Array(ca, "<", cmp) => (ca, (x: Int) => x < cmp.toInt)
        case Array(ca, ">=", cmp) => (ca, (x: Int) => x >= cmp.toInt)
        case Array(ca, "<=", cmp) => (ca, (x: Int) => x <= cmp.toInt)
        case Array(ca, "==", cmp) => (ca, (x: Int) => x == cmp.toInt)
        case Array(ca, "!=", cmp) => (ca, (x: Int) => x != cmp.toInt)
      }
      
      Instruction(commandArgument, command, conditionArgument, condition)
    }
  }
  
  text.trim().split("\n").map(parseInstruction).toList
}

case class Execution(registers: Map[String, Int], maxEver: Int)

def evaluate(instructions: List[Instruction]) = {
  val initial = Execution(Map[String, Int]().withDefaultValue(0), 0)
  instructions
    .foldLeft(initial) { (a, i) => 
      val cv = a.registers(i.conditionArgument)
      if (i.condition(cv)) {
        val v = a.registers(i.commandArgument)
        Execution(
          a.registers.updated(i.commandArgument, i.command(v)),
          if (v > a.maxEver) v else a.maxEver)
      }
      else a
    }
}

def process(text: String) = {
  val is = parseInstructions(input);
  val result = evaluate(is);
  (result.registers.values.max, result.maxEver)
}

val input = """
b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10
""" // Your input

val result = process(input)
println("d8p1> " + result._1)
println("d8p2> " + result._2)