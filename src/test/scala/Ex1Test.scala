package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex1Test:
  val input = scala.io.Source.fromFile(s"input/input_${Ex1.num}.txt")
  val result = Ex1.run(input.getLines)
  @Test def t1(): Unit = 
    assertEquals(result.part1, 1387)
    assertEquals(result.part2, 1362)
