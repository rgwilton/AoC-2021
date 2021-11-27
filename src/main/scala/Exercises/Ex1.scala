package aoc

import scala.collection.immutable.SortedSet

object Ex1 extends Exercise:
 type ParsedInput = Seq[Int]

  def parseInput(input: Iterator[String]) = input.asIntegers.toSeq

  def part1(input: ParsedInput) = 
    val target = 2020
    val inputSet = input.to(SortedSet)

    val candidates = inputSet.iterator.takeWhile(_ < target/2)
 
    candidates
      .find { x => inputSet.contains(target - x)}
      .map { x => x * (target - x) }
      .getOrElse("No match found")

  def part2(input: ParsedInput) =
    "Not Coded Yet"
