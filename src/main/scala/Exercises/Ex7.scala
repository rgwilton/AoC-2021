package aoc

import scala.collection.mutable

object Ex7 extends Exercise:
  type ParsedInput = Seq[Int]

  def parseInput(input: Iterator[String]) = input.next.asIntegers.toSeq

  extension (xs: Seq[Int])
    def mean = xs.sum/xs.length
    def median = 
      val sortedXs = xs.sorted
      sortedXs(xs.size / 2)

  def minFuel(input: ParsedInput, candidate: Int, fuelCalc: Int => Int) =
    inline def decreasing(xs: LazyList[Int]) = xs.head > xs.last

    // Select the last decrease value from increase/decreasing candidates.
    inline def candidates(step: Int = 1) = 
      LazyList.from(candidate, step).map(fuelCalc).sliding(2).dropWhile(decreasing(_)).next.head
    inline def increasingCandidates = candidates(1)
    inline def decreasingCandidates = candidates(-1)
    increasingCandidates min decreasingCandidates

  def part1(input: ParsedInput) = 
    val candidate = input.median
    def fuelCalc(target: Int) = input.foldLeft(0)((sum, x) => sum + Math.abs(target - x))
    minFuel(input, candidate, fuelCalc)

  def part2(input: ParsedInput) =
    val candidate = input.median
    inline def fuelCost(distance: Int) = ((1 + distance) * distance) / 2
    def fuelCalc(target: Int) = input.foldLeft(0)((sum, x) => sum + fuelCost(Math.abs(target - x)))
    minFuel(input, candidate, fuelCalc)

