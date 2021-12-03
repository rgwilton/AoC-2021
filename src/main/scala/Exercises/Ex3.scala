package aoc

import scala.collection.mutable

object Ex3 extends Exercise:
  type ParsedInput = (Array[Int], Int)
  def parseInput(input: Iterator[String]) = 
    val bufIter = input.buffered
    val lineLen = bufIter.head.length
    val inputArray = bufIter.map(Integer.parseInt(_, 2)).toArray
    (inputArray, lineLen)

  extension (a: Iterable[Int])
    // Convert an sequence of binary digits to an Integer
    def bitsToInt = 
      var value = 0
      for v <- a do 
        value <<= 1; value += v
      value 

  def part1(input: ParsedInput) =
    val (data, lineLen) = input
    val array = for i <- (0 until lineLen).reverse yield data.count(_.hasBitSet(i))
    val gamma = array.map(x => if x >= (data.length / 2) then 1 else 0)
    val epsilon = array.map(x => if x < (data.length / 2) then 1 else 0)
    gamma.bitsToInt * epsilon.bitsToInt

  def part2(input: ParsedInput) =
    val (data, lineLen) = input

    def worker(compareFn: (Boolean, Boolean) => Boolean) = 
      var candidates = mutable.Set(data:_*)
      var bit = lineLen - 1
        while candidates.size > 1 do
          val bitCount = candidates.count(_.hasBitSet(bit))
          val checkBit = bitCount >= (candidates.size - bitCount)
          candidates.filterInPlace { candidate => 
            compareFn(candidate.hasBitSet(bit), checkBit)
          }
          bit -= 1
        candidates.head

    val O2 = worker(_ == _)
    val CO2 = worker(_ != _)
    O2 * CO2