def findAfterLast(step: Int, count: Int) = {
  def insert(step: Int)(state: (List[Int], Int), value: Int) = {
    val (values, start) = state
    val position = (start + step) % values.size + 1
    val newValues = values.take(position) ++ List(value) ++ values.drop(position)
    (newValues, position)
  }

  val (values, position) = (1 to count).foldLeft((List(0), 0))(insert(step))
  values(position + 1)
} 

def findAfterZero(step: Int, count: Int) = {
  def makeStep(step: Int)(state: (Int, Int, Int), value: Int) = {
    val (size, target, start) = state
    val position = (start + step) % size + 1
    (size + 1, if (position == 1) value else target, position)
  }
  
  (2 to count).foldLeft((2, 1, 1))(makeStep(step))._2
} 


val input = 304 // Your input
println(findAfterLast(input, 2017))
println(findAfterZero(input, 5000000))