package aoc

import org.junit.Test
import org.junit.Assert.*

object TestCase:
  def apply(input: String, p1Res: Any, p2Res: Any): TestCase = 
    TestCase(input.stripMargin.linesIterator, p1Res, p2Res)

case class TestCase(input: Iterator[String], p1Res: Any, p2Res: Any)

abstract class ExTest(exercise: Exercise):
  def exInput = exercise.input
  def testcases: Seq[TestCase]

  def runTest(tc: TestCase) = 
    val result = exercise.run(tc.input)
    assertEquals(result.part1, tc.p1Res)
    assertEquals(result.part2, tc.p2Res)

  @Test def test: Unit = 
    for tc <- testcases do runTest(tc)
