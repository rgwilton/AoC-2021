package aoc

class Ex3Test extends ExTest(Ex3):
  val testcases = Seq(
    TestCase(
    """|00100
       |11110
       |10110
       |10111
       |10101
       |01111
       |00111
       |11100
       |10000
       |11001
       |00010
       |01010""", 198, 230),
    TestCase(exInput, 3885894, 4375225)
  )
