package aoc

import scala.collection.mutable

object Ex4 extends Exercise:
  type ParsedInput = (Array[Int], Seq[Card])

  object Card:
    final val rowMask = Integer.parseInt("11111", 2)
    final val colMask = Integer.parseInt("00001" * 5, 2)

  class Card(val nums: Array[Int]):
    var matched = 0

    def isBingoRow(row: Int) =
      inline def mask = Card.rowMask << (5 * row)
      mask == (matched & mask)

    def isBingoColumn(column: Int) =
      inline def mask = Card.colMask << column
      mask == (matched & mask)

    def sum = 
      // Sums the nums that don't have a bit set in 'matched'
      (0 until nums.length).filterNot(matched.hasBitSet).map(nums).sum

    // Update the card with one called number.  Returns whether a card is Bingo.
    def check(called: Int): Boolean = 
      val idx = nums.indexOf(called)
      if idx >= 0 then
        matched |= 1 << idx
        isBingoRow(idx / 5) || isBingoColumn(idx % 5)
      else
        false

    // Check all numbers and return the calledNum and index of the calledNum.
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
        x.sum * num
    res.head

  def part2(input: ParsedInput) =
    val (calledNums, cards) = input
    val remCards = mutable.Set(cards:_*)
    var remNums = calledNums.toList

    val (idx, (card, num)) = 
      cards.flatMap { card =>
        card.checkAll(calledNums) match 
          case Some(num, idx) =>
            Some(idx -> (card, num))
          case None => None
      }.sortBy(x => x._1).last

    card.sum * num