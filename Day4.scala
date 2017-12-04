def parseMultiline(text: String) = {
  text
    .trim()
    .split("\n")
    .map(_.trim().split("\\s+").toList)
}

def countValidPhrases[T](f: String => T)(text: String) = {
  parseMultiline(text)
  .filter(phrase => phrase.length == phrase.map(f).distinct.length)
  .length
}

def countUniqueWords = countValidPhrases(identity)(_)
def countUniqueAnagrams = countValidPhrases(_.toList.sorted)(_)

val input = """
aa bb cc
aa bb cc aa
ab bc cd ba
"""

println("d4p1> " + countUniqueWords(input))
println("d4p2> " + countUniqueAnagrams(input))