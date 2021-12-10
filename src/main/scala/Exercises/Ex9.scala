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
  def part2(input: ParsedInput) =
    type BasinId = Int
  
    object BasinRange:
      var nextId = 0
      def apply(start: Int, end: Int) =
        nextId += 1 
        new BasinRange(start, end, nextId -1)
    case class BasinRange(start: Int, end: Int, var id: BasinId):
      def length = end - start + 1
      def overlaps(b: BasinRange) =
        (start <= b.start && b.start <= end)
        || (start <= b.end && b.end <= end)
        || (b.start <= start && start <= b.end)
         || (b.start <= end && end <= b.end)
    val basins = mutable.Map[BasinId, Int]()

    var previousSegs = Seq[BasinRange]()
    for row <- input do
      def getNextSegs(prev9idx: Int, i: Int, segs: Seq[BasinRange]): Seq[BasinRange] =
        if i == row.length then segs
        else if row(i) < 9 then getNextSegs(prev9idx, i + 1, segs)
        else // row(i) == 9
          if i > (prev9idx + 1) then
            getNextSegs(prev9idx = i, i + 1, segs :+ BasinRange(prev9idx + 1, i - 1))
          else 
            getNextSegs(prev9idx = i, i + 1, segs)

      val nextSegs = getNextSegs(0, 1, Seq())
      
      for seg <- nextSegs do
        basins(seg.id) = seg.length
        for overlapSeg <- previousSegs.filter(seg.overlaps) do
          basins(seg.id) += basins.get(overlapSeg.id).getOrElse(0)
          basins.remove(overlapSeg.id)
          nextSegs.filter(_.id == overlapSeg.id).map(_.id = seg.id)
          overlapSeg.id = seg.id
      end for
      previousSegs = nextSegs.toSeq
    basins.values.toSeq.sorted(using Ordering.Int.reverse).take(3).product
