def anticapcha(capcha: String, index: Int) = {
  val list = capcha toList
  
  val (part1, part2) = list splitAt index
  val rotatedList = part2 ++ part1
  
  list
    .zip(rotatedList)
    .filter(x => x._1 == x._2)
    .map(_._1.asDigit)
    .sum
}

val capcha = "1111" // your input
println("d1p1> " + anticapcha(capcha, 1))
println("d1p2> " + anticapcha(capcha, capcha.length / 2))