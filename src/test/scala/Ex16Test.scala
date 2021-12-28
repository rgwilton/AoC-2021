package aoc

class Ex16Test extends ExTest(Ex16):
  val testcases = Seq(
    TestCase(
     """|D2FE28""", 6, 2021L),
     TestCase(
      """|38006F45291200""", 9, 1L),
     TestCase(
      """|EE00D40C823060""", 14, 3L),
      TestCase(
       """|8A004A801A8002F478""", 16, 15L),
     TestCase(
      """|C200B40A82""", 14, 3L),
     TestCase(
      """|04005AC33890""", 8, 54L),
     TestCase(
      """|880086C3E88112""", 15, 7L),
     TestCase(
      """|CE00C43D881120""", 11, 9L),
     TestCase(
      """|D8005AC2A8F0""", 13, 1L),
     TestCase(
      """|F600BC2D8F""", 19, 0L),
     TestCase(
      """|9C005AC2F8F0""", 16, 0L),
     TestCase(exInput, 960, 12301926782560L)
  )
