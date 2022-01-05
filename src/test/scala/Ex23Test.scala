package aoc

class Ex23Test extends ExTest(Ex23b):
  val testcases = Seq(
    TestCase(
       """|#############
          |#...........#
          |###B#C#B#D###
          |  #A#D#C#A#
          |  #########""", 12521, 44169),
    TestCase(exInput, 19167, 47665)
  )
