package aoc

class Ex15Test extends ExTest(Ex15):
  val testcases = Seq(
    TestCase(
     """|1163751742
        |1381373672
        |2136511328
        |3694931569
        |7463417111
        |1319128137
        |1359912421
        |3125421639
        |1293138521
        |2311944581""", 40, 315),
    TestCase(exInput, 398, 2817)
  )
