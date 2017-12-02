def parseMultilineNumbers(text: String) = {
  text
    .trim()
    .split("\n")
    .map(_.trim().split("\\s+").map(_.toInt).toList)
}

def minMaxDiff(numbers: List[Int]) = 
  numbers.max - numbers.min
  
def devisable(numbers: List[Int]) = {
  val matches =
    for (n1 <- numbers;
         n2 <- numbers
         if n1 > n2 && n1 % n2 == 0)
    yield n1 / n2
  matches.head
}

def checksum(spreadsheet: String, reducer: List[Int] => Int) = {
  parseMultilineNumbers(spreadsheet)
    .map(reducer)
    .sum
}

val spreadsheet = """
5 9 2 8
9 4 7 3
3 8 6 5
""" // Your input goes here
println("d3p1> " + checksum(spreadsheet, minMaxDiff))
println("d3p2> " + checksum(spreadsheet, devisable))
