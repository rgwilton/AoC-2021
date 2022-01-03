package aoc

import scala.collection.mutable
import math.Ordered.orderingToOrdered

object Ex19 extends Exercise:
  enum Transform:
    case ToX
    case ToMinusX
    case ToY 
    case ToMinusY
    case ToZ
    case ToMinusZ
  import Transform._

  case class Transform3D(xOffset: Int, xFrom: Transform, yOffset: Int, yFrom: Transform, zOffset: Int, zFrom:Transform)

  case class Coord(x: Int, y: Int, z: Int) extends Ordered[Coord]:
    def compare(that: Coord): Int =
      (this.x, this.y, this.z) compare (that.x, that.y, that.z)

    def manhattanDist(that: Coord): Int =
      ((this.x max that.x) - (this.x min that.x)) +
      ((this.y max that.y) - (this.y min that.y)) +
      ((this.z max that.z) - (this.z min that.z))

    def transform(t: Transform3D): Coord =
      def get(offset: Int, t: Transform): Int = 
        t match
          case ToX => x - offset
          case ToMinusX => -x - offset
          case ToY => y - offset
          case ToMinusY => -y - offset
          case ToZ => z - offset
          case ToMinusZ => -z - offset
      Coord(get(t.xOffset, t.xFrom), get(t.yOffset, t.yFrom), get(t.zOffset, t.zFrom))

  case class Scanner(id: Int, coords: IndexedSeq[Coord]):
    val xCoords = coords.map(_.x).toArray.sorted
    val xCoordsRev = xCoords.map(-_).reverse
    val yCoords = coords.map(_.y).toArray.sorted
    val yCoordsRev = yCoords.map(-_).reverse
    val zCoords = coords.map(_.z).toArray.sorted
    val zCoordsRev = zCoords.map(-_).reverse

    // Return the reverse coordiantes.
    val revCoords =
      Map(xCoords -> xCoordsRev,
          xCoordsRev -> xCoords,
          yCoords -> yCoordsRev,
          yCoordsRev -> yCoords,
          zCoords -> zCoordsRev,
          zCoordsRev -> zCoords)

    val candidates = Map(xCoords -> ToX,
                         xCoordsRev -> ToMinusX,
                         yCoords -> ToY,
                         yCoordsRev -> ToMinusY,
                         zCoords -> ToZ,
                         zCoordsRev -> ToMinusZ)

  def overlaps(src: Array[Int], tgt: Array[Int]) =
    val candidateOffsets = 
      for i <- 0 to (src.length/2) + 1
          j <- 0 to (tgt.length/2) + 1 yield tgt(j) - src(i)

    candidateOffsets.distinct.flatMap { offset =>
      var count = 0
      var x = 0
      var y = 0
      while (x < src.length && y < tgt.length) do
        val offsetX = src(x) + offset
        if offsetX < tgt(y) then x += 1
        else if offsetX > tgt(y) then y += 1
        else 
          count += 1
          x += 1
      if (count >= 12) Some(offset) else None
    }
    
  def overlappingScanner(src: Scanner, tgt: Scanner) =
    // Try and relate X, and if that succeeds then Y followed by Z.
    val xCands = tgt.candidates
    var results = List[Transform3D]()
    var xCandOverlapCount = 0
    for (xTgtCoords, xTransform) <- xCands do
      val xCand = overlaps(src.xCoords, xTgtCoords)
      if xCand.nonEmpty then
        xCandOverlapCount += 1
        val yCands = xCands -- Iterable(xTgtCoords, tgt.revCoords(xTgtCoords))
        for (yTgtCoords, yTransform) <- yCands do
          val yCand = overlaps(src.yCoords, yTgtCoords)
          if yCand.nonEmpty then
            val zCands = yCands -- Iterable(yTgtCoords, tgt.revCoords(yTgtCoords))
            for (zTgtCoords, zTransform) <- zCands do
              val zCand = overlaps(src.zCoords, zTgtCoords)
              if zCand.nonEmpty then
                results ::= Transform3D(xCand.head, xTransform, yCand.head, yTransform, zCand.head, zTransform)
    results

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

  // Store for part 2.
  var scannerLocations = mutable.Set[Coord](Coord(0, 0, 0))
  def part1(input: Seq[Scanner]) = 
    var coords = mutable.Set[Coord](input.head.coords:_*)
    var unknown = input.tail.to(mutable.Set)

    // Find overlapping scanners, updating coords and scannerLocations.
    def findOverlaps(src: Scanner, transforms: List[Transform3D]): Unit =
      unknown.foreach { tgt =>
        val res = overlappingScanner(src, tgt)
        if res.nonEmpty then
          unknown -= tgt
          val newTransforms = res.head :: transforms
          tgt.coords.foreach(coord =>
            val transformedCoord = newTransforms.foldLeft(coord)( (coord, t) => coord.transform(t))
            coords.add(transformedCoord)
          )
          val scannerLocation = newTransforms.foldLeft(Coord(0,0,0))( (coord, t) => coord.transform(t))
          scannerLocations += scannerLocation

          findOverlaps(tgt, newTransforms)
      }

    findOverlaps(input.head, List())
    coords.size

  def part2(input: ParsedInput) =
    scannerLocations.toSeq.combinations(2).map {
      case Seq(a, b) => a.manhattanDist(b)
    }.max