package aoc

class Ex23Test extends ExTest(Ex23):
  val testcases = Seq(
    TestCase(
       """|#############
          |#...........#
          |###B#C#B#D###
          |  #A#D#C#A#
          |  #########""", 12521, ""),
    TestCase(exInput, 19167, "")
  )
