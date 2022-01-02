package aoc

class Ex17Test extends ExTest(Ex17):
  val testcases = Seq(
    TestCase(
     """target area: x=20..30, y=-10..-5""", 45, 112),
    TestCase(exInput, 5671, 4556)
  )
