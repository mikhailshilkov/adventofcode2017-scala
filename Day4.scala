def parseMultiline(text: String) = {
  text
    .trim()
    .split("\n")
    .map(_.trim().split("\\s+").toList)
}

def countValidPhrases[T](text: String, f: String => T) = {
  parseMultiline(text)
  .filter(s => s.length == s.map(f).distinct.length)
  .length
}

def countUniqueWords(text: String) = 
  countValidPhrases(text, identity)

def countUniqueAnagrams(text: String) = 
  countValidPhrases(text, _.toList.sorted)

val input = """
aa bb cc
aa bb cc aa
ab bc cd ba
"""

println("d4p1> " + countUniqueWords(input))
println("d4p2> " + countUniqueAnagrams(input))