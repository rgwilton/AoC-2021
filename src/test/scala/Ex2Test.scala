package aoc

class Ex2Test extends ExTest(Ex2):
  val testcases = Seq(
    TestCase(
    """|forward 5
       |down 5
       |forward 8
       |up 3
       |down 8
       |forward 2""", 150, 900),
    TestCase(Ex2.input, 1451208, 1620141160)
  )
