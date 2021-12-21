package aoc

class Ex21Test extends ExTest(Ex21):
  val testcases = Seq(
    TestCase(
       """|Player 1 starting position: 4
          |Player 2 starting position: 8""", 739785, 444356092776315L),
    TestCase(exInput, 506466, 632979211251440L)
  )
