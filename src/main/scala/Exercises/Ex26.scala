package aoc

import scala.collection.mutable

import scala.util.Random
import scala.collection.parallel.mutable.ParArray

object Ex26 extends Exercise:
  override def input = Iterator[String]()
  type ParsedInput = Array[Int]
  val maxInt = 100_000
  val thresholds = ParArray.fill(1_000) { maxInt/2 - 5 + Random.nextInt(10)}
  def parseInput(input: Iterator[String]) = 
    var i = 0
    Array.fill(50_000){ /*i += 1; if i % 2 == 0 then 0 else maxInt } */Random.nextInt(maxInt)}

  def calc(nums: Array[Int]) = 
    for threshold <- thresholds yield
      nums.count(_ < threshold)

  def part1(input: ParsedInput) =
    calc(input).sum

  def part2(input: ParsedInput) =
    calc(input.sorted).sum