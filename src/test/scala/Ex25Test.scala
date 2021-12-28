package aoc

class Ex25Test extends ExTest(Ex25):
  val testcases = Seq(
    TestCase(
       """|v...>>.vv>
          |.vv>>.vv..
          |>>.>v>...v
          |>>v>>.>.v.
          |v>v.vv.v..
          |>.>>..v...
          |.vv..>.>v.
          |v.v..>>v.v
          |....v..v.>""", 58, ""),
    TestCase(exInput, 486, "")
  )
