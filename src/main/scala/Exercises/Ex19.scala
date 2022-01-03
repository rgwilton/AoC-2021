package aoc

import scala.collection.mutable
import math.Ordered.orderingToOrdered

object Ex19 extends Exercise:
  case class Coord(x: Int, y: Int, z: Int) extends Ordered[Coord]:
    def compare(that: Coord): Int =
      (this.x, this.y, this.z) compare (that.x, that.y, that.z)

    def toOrderedAbs = 
      val Seq(a, b, c) = Seq(x.abs, y.abs, z.abs)
      Coord(a, b, c)

    def - (that: Coord) =
      Coord(this.x - that.x, this.y - that.y, this.z - that.z)

  // Sort into increasing X, Y, Z
  // Caculate the distances between the ordered consecutive points.
  case class Scanner(id: Int, coords: IndexedSeq[Coord]):
    val orderedX = coords.toIndexedSeq.sortBy(_.x)
    val xDiffs = orderedX.sliding(2).map{ cpair => cpair(1).x - cpair(0).x }.toIndexedSeq
    val xDiffRev = xDiffs.reverse

    val orderedY = coords.toIndexedSeq.sortBy(_.y)
    val yDiffs = orderedY.sliding(2).map{ cpair => cpair(1).y - cpair(0).y }.toIndexedSeq
    val yDiffRev = yDiffs.reverse

    val orderedZ = coords.toIndexedSeq.sortBy(_.z)
    val zDiffs = orderedZ.sliding(2).map{ cpair => cpair(1).z - cpair(0).z }.toIndexedSeq
    val zDiffRev = zDiffs.reverse

  def overlaps(src: IndexedSeq[Coord], dst: IndexedSeq[Coord], reverse: Boolean) =
    for i <- 0 to src.length - 12 do
      ???

  type ParsedInput = Seq[Scanner]

  val ScannerR = """--- scanner (\d+) ---""".r
  
  def parseInput(input: Iterator[String]) =
    val scanners =
      for line <- input yield
        val ScannerR(digits) = line
        var coords =
          for line <- input.takeWhile(_ != "") yield
            val Array(x, y, z) = line.split(',')
            Coord(x.toInt, y.toInt, z.toInt)
        Scanner(digits.toInt, coords.toIndexedSeq)
    scanners.toSeq

  def part1(input: Seq[Scanner]) = 
    // println(s"${input.length}")
    // for (scanner <- input)
    //   println(s"  ${scanner.coords.length}")      
    // 0

    // Convert coordinates to absolutes, order
    // val orderedCoords = 
    //   for scanner <- input yield
    //     val sorted = scanner.coords.map(_.toOrderedAbs).sorted
    //     val offset = sorted.head
    //     scanner.id -> sorted.map(_ - offset)

    //val scannerMap = orderedCoords.toMap
    // for scanner <- orderedCoords do
    //   println(s"Scanner: ${scanner._1}")
    //   for coord <- scanner._2 do
    //     println(s"  Coord: ${coord}")

    ""

  def part2(input: ParsedInput) =
    ""