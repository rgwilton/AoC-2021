package aoc

import scala.collection.mutable
import scala.collection.mutable.Buffer

object Ex9 extends Exercise:
  type ParsedInput = IndexedSeq[IndexedSeq[Int]]

  // ParsedInput is as a 2D array of characters with a boarder of '9' characters.
  def parseInput(input: Iterator[String]) = 
    val bufIter = input.buffered
    val len = bufIter.head.length
    val ninesRow = for i <- 1 to len + 2 yield 9
    def rows = bufIter.map { line => IndexedSeq(9) ++ line.map(_ - '0').toSeq ++ Seq(9) }
    IndexedSeq(ninesRow) ++ rows ++ Seq(ninesRow)


  def part1(input: ParsedInput) = 
    //for row <- input do
    //  println(row.mkString(""))
    def isMin(row: Int, col: Int) =
      val x = input(row)(col)
      x < input(row - 1)(col)
      && x < input(row + 1)(col)
      && x < input(row)(col - 1)
      && x < input(row)(col + 1)
    
    val lowPoints =
      for row <- 1 until input.length - 1
          col <- 1 until input(0).length - 1
          if isMin(row, col) yield 1 + input(row)(col)
    lowPoints.sum

  // Convert each row into ranges of candidates between 9's.
  def part2(input: ParsedInput) = ""
    // var nextId = 0
    // val basins = Buffer[Int]()
    // case class BasinRange(start: Int, end: Int, var id: Int):
    //   //def updateId
    // var previousSegs = Seq[BasinRange]()
    // for row <- input do
    //   var prev9idx = 0
    //   for i <- 1 until row.length yield
    //     if row(i) == 9 then
    //         if i > prev9idx + 1 then
    //           val (start, end) = (prev9idx + 1, i - 1)
    //           var id = -1
    //           previousSegs.collect {
    //             case b@BasinRange(s, e, i) if end >= s || start <= e =>
    //               if id == -1 then id = i
    //               else b.id = id
    //           }
    //           if id == -1 then id = nextId; nextId += 1
    //           BasinRange(start, end, id)
    //         prev9idx == i
            