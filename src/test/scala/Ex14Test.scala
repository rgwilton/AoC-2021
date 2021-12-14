package aoc

class Ex14Test extends ExTest(Ex14):
  val testcases = Seq(
    TestCase(
     """|NNCB
        |
        |CH -> B
        |HH -> N
        |CB -> H
        |NH -> C
        |HB -> C
        |HC -> B
        |HN -> C
        |NN -> C
        |BH -> H
        |NC -> B
        |NB -> B
        |BN -> B
        |BB -> N
        |BC -> B
        |CC -> N
        |CN -> C""", 1588L, 2188189693529L),
    TestCase(exInput, 2768L, 2914365137499L)
  )
