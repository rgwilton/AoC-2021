package aoc

import scala.collection.mutable

object Ex6 extends Exercise:
  type ParsedInput = Seq[Int]

  def parseInput(input: Iterator[String]) = input.next.asIntegers

  def calcFishCount(input: ParsedInput, days: Int): Long =
    val initial = for i <- 1 to 5 yield input.count(_ == i).toLong
    val q = mutable.Queue(0L) ++ initial ++ Seq(0L, 0L, 0L)
    for d <- 1 to days do
      // Day 0 fish are added to day 6 fish and become day 8 fish.
      // Every other fish becomes one day closer to spawning a new fish.
      val d0Fish = q.dequeue
      q.enqueue(d0Fish)
      q.update(6, q(6) + d0Fish)
    q.sum

  def part1(input: ParsedInput) = calcFishCount(input, 80)

  def part2(input: ParsedInput) = calcFishCount(input, 256)