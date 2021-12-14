package aoc

import scala.collection.mutable

object Ex14 extends Exercise:
  type RulesMap = Map[String, String]
  case class ParsedInput(template: String, rules: RulesMap, allChars: Set[Char])
  //type ParsedInput = Input

  def parseInput(input: Iterator[String]) = 
    import scala.language.unsafeNulls
    val template = input.next()
    input.next() // skip blank line
    var allChars = template.toSet
    val rules = 
      for line <- input
          Array(a, b) = line.split(" -> ") yield
            allChars ++= a.toCharArray ++ b
            (a -> b)

    ParsedInput(template, rules.toMap, allChars)

  // Initial version for part 1.
  def doPolymerSteps(str: String, rules: RulesMap, steps: Int): String =
    def nextStr =
      str
      .sliding(2)
      .map { c => rules(c) + c(1) }
      .mkString(str(0).toString, "", "")

    if steps == 0 then str
    else doPolymerSteps(nextStr, rules, steps - 1)

  def countCharsSeq(str: String, chars: Set[Char]):Seq[Int] =
     chars
      .map { ch => str.iterator.count(_ == ch)}
      .toSeq
      .sorted

  // Faster solution that doesn't build the polymer strings, just
  // used for part 1 and part 2
  def calcPolymerLetterCounts(template: String, rules: RulesMap, count: Int): Map[Char, Long] =

    case class RuleCount(depOnPairs: List[String], count: Long)
    val EmptyRuleCount = RuleCount(List(), 0)

    type CountMap = Map[String, RuleCount]
    var countMap: CountMap = Map().withDefaultValue(EmptyRuleCount)

    // Set up the map based on the rules.
    rules.foreach{ (pair, char) =>
      val newPairs = Seq(pair(0) + char, char.toString + pair(1))
      for newPair <- newPairs do
        val rc = countMap(newPair)
        countMap += newPair -> RuleCount(pair :: rc.depOnPairs, 0)
    }

    // Setup the initial counts based on the input template.
    for pair <- template.sliding(2)
        rcEntry = countMap(pair) do
      countMap += pair -> rcEntry.copy(count = rcEntry.count + 1)
    
    def step(curCountMap: CountMap): CountMap =
        curCountMap.map { (key, rc) =>
          val newCount = 
            rc.depOnPairs
            .map { depPair => curCountMap(depPair).count }
            .sum
          key -> rc.copy(count = newCount)
        }.withDefaultValue(EmptyRuleCount)

    for i <- 1 to count do 
      countMap = step(countMap)

    // Attribute counts for each pair to both letters, but this double counts, so divide by two,
    // taking into account the end of the chain that will be odd.
    val letterCounts = 
      countMap
      .toSeq
      .flatMap { (pair, rc) => pair.map { ch => ch -> rc.count } }
      .groupMapReduce(_._1)(_._2)(_ + _) // Sum by value.
      .mapValues{ count => (count + 1) / 2 }
      .toMap
    letterCounts


  def part1(input: ParsedInput) = 
    val letterCounts = 
      calcPolymerLetterCounts(input.template, input.rules, 10)
      .values
      .toSeq
      .sorted
    
    letterCounts.max - letterCounts.min

  def part2(input: ParsedInput) =
    // Construct a map of what dependencies each character pair has.
    val letterCounts = 
      calcPolymerLetterCounts(input.template, input.rules, 40)
      .values
      .toSeq
      .sorted
    
    letterCounts.max - letterCounts.min
