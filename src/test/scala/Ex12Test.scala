package aoc

class Ex12Test extends ExTest(Ex12):
  val testcases = Seq(
    TestCase(
     """|start-A
        |start-b
        |A-c
        |A-b
        |b-d
        |A-end
        |b-end""", 10, 36),
    TestCase(
     """|dc-end
        |HN-start
        |start-kj
        |dc-start
        |dc-HN
        |LN-dc
        |HN-end
        |kj-sa
        |kj-HN
        |kj-dc""", 19, 103),      
    TestCase(exInput, 3421, 84870)
  )
