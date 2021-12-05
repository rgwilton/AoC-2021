package aoc

import scala.collection.mutable

object Ex4 extends Exercise:
  type ParsedInput = (Array[Int], Seq[Card])

  object Card:
    final val rowMask = Integer.parseInt("11111", 2)
    final val colMask = Integer.parseInt("100001000010000100001", 2)

  class Card(val nums: Array[Int]):
    var matched = 0

    def isBingoRow(row: Int) =
      inline def mask = Card.rowMask << row
      mask == (matched & mask)

    def isBingoColumn(column: Int) =
      inline def mask = Card.colMask << column
      mask == (matched & mask)

    def sum = 
      (0 until nums.length).filterNot(matched.hasBitSet).map(nums).sum

    def check(called: Int): Boolean = 
      val idx = nums.indexOf(called)
      if idx >= 0 then
        matched |= 1 << idx
        isBingoRow(idx / 5) || isBingoColumn(idx % 5)
      else
        false

    def checkAll(calledNums: Array[Int]) =
      calledNums.iterator.zipWithIndex.find {
        (calledNum, idx) => check(calledNum)
      }

  def parseInput(input: Iterator[String]) = 
    val calledNums = input.next().split(',').map(_.toInt)
    input.next() // Skip blank line

    val res = 
      input.grouped(6).map { lines =>
        val cardNums =
          for line <- lines.toArray.takeWhile(_ != "")
            int <- line.grouped(3).map(_.strip.nn.toInt) yield int
        Card(cardNums)
      }

    (calledNums, res.toSeq)

  def part1(input: ParsedInput) = 
    val (calledNums, cards) = input
    val res =
      for num <- calledNums.view
          x <- cards.find(card => card.check(num)) yield
        // println(x.matched.toBinaryString.reverse.grouped(5).mkString("\n"))
        // println(x.nums.mkString(","))
        // println(num)
        x.sum * num
    res.head

    // for card <- cards do
    //     println(card.matched.toBinaryString.reverse.grouped(5).mkString("\n"))
    //     println(card.nums.mkString(","))      
    // res.head

  def part2(input: ParsedInput) = ""