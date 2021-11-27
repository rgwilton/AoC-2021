package aoc

import scala.annotation.tailrec

object Ex2 extends Exercise:
  type ParsedInput = Seq[Int]

  def parseInput(input: Iterator[String]) = input.asIntegers.toSeq

  final def calcFuel(x: Int) = (x/3) - 2

  def part1(input: ParsedInput) = 
    input.map(calcFuel).sum

  def part2(input: ParsedInput) =
    @tailrec 
    def calcAllFuel(x: Int, total: Int): Int = 
      if x < 9 then total else
        val f = calcFuel(x)
        calcAllFuel(f, total + f)
    input.map(calcAllFuel(_, 0)).sum

