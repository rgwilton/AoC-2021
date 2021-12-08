package aoc

import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Success
import scala.util.Failure

// Make the solution faster (the baseline was approx 7 ms) by:
//   Tweaking performance during parsing (i.e., get it to generate to a Seq)
//   Reusing the same Map for both parts (avoids redoing work).
//   Use the same logic, but write it out in a more imperative way.
//  Striping the sum calculation across multiple threads.
object Ex5b extends Exercise:
  type ParsedInput = Seq[Line]
  override def input = scala.io.Source.fromFile(s"input/input_5.txt").getLines

  final case class Line(x1: Int, y1: Int, x2: Int, y2: Int):
    inline def isHorizontal = y1 == y2
    inline def isVertical = x1 == x2
    inline def isDiagonal = !(isHorizontal || isVertical)

  final case class Map(xMax: Int, yMax: Int):
    val map = Array.ofDim[Int](xMax, yMax)

    inline def range(m: Int, n: Int) = 
      if m < n then m to n else m to (n, step = -1)

    inline def drawHorizontalLine(l: Line) =
      var x = l.x1
      var y = l.y1
      if l.x1 <= l.x2 then
        while x <= l.x2 do
          map(x)(y) += 1
          x += 1;
      else // l.x1 > l.x2
        while x >= l.x2 do
          map(x)(y) += 1
          x -= 1;
          
    inline def drawVerticalLine(l: Line) =
      var x = l.x1
      var y = l.y1
      if l.y1 <= l.y2 then
        while y <= l.y2 do
          map(x)(y) += 1
          y += 1;
      else // l.y1 > l.y2
        while y >= l.y2 do
          map(x)(y) += 1
          y -= 1;

    inline def drawDiagonalLine(l: Line) =
      var x = l.x1
      var y = l.y1
      if l.x1 <= l.x2 then
        if l.y1 <= l.y2 then
          while x <= l.x2 do
            map(x)(y) += 1
            x += 1; y += 1
        else // l.y1 > l.y2
          while x <= l.x2 do
            map(x)(y) += 1
            x += 1; y -= 1
      else // l.x1 > l.x2
        if l.y1 <= l.y2 then
          while x >= l.x2 do
            map(x)(y) += 1
            x -= 1; y += 1
        else // l.y1 > l.y2
          while x >= l.x2 do
            map(x)(y) += 1
            x -= 1; y -= 1

    def parCountFuture(fromRow: Int, toRow: Int) =
      Future {
        var c = 0
        for x <- fromRow until toRow
            y <- 0 until yMax do
          if map(x)(y) > 1 then c += 1
        c
      }

    def parCount(parThreads: Int): Int = 
      val increment = 1000 / parThreads
      var startRow = 0
      var endRow = startRow + increment
      val fs = 
        Future.sequence {
          (1 to parThreads).map { i =>
            val res = parCountFuture(startRow, endRow)
            startRow = endRow
            if i < (parThreads -1) then
              endRow += increment
            else
              endRow = 1000
            res
          }
        }
      val res = Await.result(fs, 10 minutes)
      res.sum
    
    def print() =
      for y <- 0 until yMax do
        println((for x <- 0 until xMax yield map(x)(y).toString).mkString(""))


  var m: Map = Map(1, 1)

  final val LineR = raw"(\d+),(\d+) -> (\d+),(\d+)".r
  def parseInput(input: Iterator[String]) = 
    // Initialize the map only once.
    m = Map(1000, 1000)
    val lines = 
      for LineR(x1, y1, x2, y2) <- input yield 
      Line(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
    lines.toSeq

  def part1(input: ParsedInput) = 
    input.filter(_.isHorizontal).foreach(x => m.drawHorizontalLine(x))
    input.filter(_.isVertical).foreach(x => m.drawVerticalLine(x))
    m.parCount(12)

  def part2(input: ParsedInput) =
    input.filter(_.isDiagonal).foreach(x => m.drawDiagonalLine(x))
    m.parCount(12)