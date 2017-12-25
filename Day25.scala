case class StateValue(writeValue: Int, moveBy: Int, continueToState: Char)
case class State(value0: StateValue, value1: StateValue)
case class Blueprints(states: Map[Char, State], count: Int)

def parseBlueprints(text: String) = {
  def parseState(lines: Array[String], startAt: Int) = {
    def parseStateValue(i: Int) = {
      val writeValue = if (lines(i) contains "1") 1 else 0
      val moveBy = if (lines(i + 1) contains "right") 1 else -1
      val continueTo = lines(i + 2).split(" Continue with state ")(1)(0)
      StateValue(writeValue, moveBy, continueTo)
    }
    val stateName = lines(startAt).split("In state ")(1)(0)
    stateName -> State(parseStateValue(startAt + 2), parseStateValue(startAt + 6))
  }
  val lines = text.trim().split("\n").map(_.trim())
  val states = lines.zipWithIndex.filter(_._1.startsWith("In state")).map(_._2)
  val count = lines(1).split("after ")(1).split(" steps")(0).toInt
  Blueprints(states.map(i => parseState(lines, i)).toMap, count)
}

def run(blueprints: Blueprints) = {
  def step(tape: Map[Int, Int], cursor: Int, stateName: Char) = {
    val state = blueprints.states(stateName)
    val currentValue = tape(cursor)
    val stateValue = if (currentValue == 0) state.value0 else state.value1
    val newTape = tape + (cursor -> stateValue.writeValue)
    val newCursor = cursor + stateValue.moveBy
    val newState = stateValue.continueToState
    (newTape, newCursor, newState)
  }
  
  val initial = (Map[Int, Int]().withDefaultValue(0), 0, 'A')
  val end = 
    (1 to blueprints.count)
    .foldLeft(initial) { case ((t, c, s), _) => step(t, c, s) }
  end._1.values.count(_ == 1)
}

val input = """
Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.
""" // Your input

println("d25> " + run(parseBlueprints(input)))