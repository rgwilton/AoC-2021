package aoc

import scala.collection.mutable
import scala.annotation.switch

object Ex22 extends Exercise:
  type ParsedInput = Seq[Cube]
  enum Dimension:
    case X, Y, Z
  import Dimension._

  case class Cube(on: Boolean, xMin: Int, xMax: Int, yMin: Int, yMax: Int, zMin: Int, zMax: Int):
    def off = !on
    def size = (1L + xMax - xMin) * (1L + yMax - yMin) * (1L + zMax - zMin)

    inline def dMin(using dim: Dimension) =
      dim match
        case X => xMin
        case Y => yMin
        case Z => zMin

    inline def dMax(using dim: Dimension) =
      dim match
        case X => xMax
        case Y => yMax
        case Z => zMax

    // Check for overlap in a given dimension.
    inline def overlaps(c: Cube, dim: Dimension): Boolean =
      given Dimension = dim
      (dMin <= c.dMin && c.dMin <= dMax)
      || (dMin <= c.dMax && c.dMax <= dMax)
      || (c.dMin <= dMin && dMin <= c.dMax)

    def overlaps(c: Cube): Boolean =
      overlaps(c, X) && overlaps(c, Y) && overlaps(c, Z)

    inline def trimDMin(min: Int)(using dim: Dimension) =
      dim match
        case X => this.copy(xMin = min)
        case Y => this.copy(yMin = min)
        case Z => this.copy(zMin = min)

    inline def trimDMax(max: Int)(using dim: Dimension) =
      dim match
        case X => this.copy(xMax = max)
        case Y => this.copy(yMax = max)
        case Z => this.copy(zMax = max)

    def split(c: Cube, dim: Dimension): (Cube, Cube, Seq[Cube], Seq[Cube]) =
      given Dimension = dim
      var candidateThis = this
      var candidateC = c
      var nonOverlappingThis = List[Cube]()
      var nonOverlappingC = List[Cube]()

      // Split on min value.
      if dMin < c.dMin && c.dMin <= dMax then
        nonOverlappingThis ::= candidateThis.trimDMax(c.dMin - 1)
        candidateThis = candidateThis.trimDMin(c.dMin)
      else if c.dMin < dMin && dMin <= c.dMax then
        nonOverlappingC ::= candidateC.trimDMax(dMin - 1)
        candidateC = candidateC.trimDMin(dMin)

      // Split on max value
      if dMin <= c.dMax && c.dMax < dMax then
        nonOverlappingThis ::= candidateThis.trimDMin(c.dMax + 1)
        candidateThis = candidateThis.trimDMax(c.dMax)
      else if c.dMin <= dMax && dMax < c.dMax then
        nonOverlappingC ::= candidateC.trimDMin(dMax + 1)
        candidateC = candidateC.trimDMax(dMax)
      (candidateThis, candidateC, nonOverlappingThis, nonOverlappingC)

    // Splits overlapping cubes.
    def split(candidate: Cube): (Cube, Seq[Cube], Seq[Cube]) =
      val (candidateYThis, candidateY, nonOverlappingXThis, nonOverlappingXC) = 
        this.split(candidate, X)
      val (candidateZThis, candidateZ, nonOverlappingYThis, nonOverlappingYC) = 
        candidateYThis.split(candidateY, Y)
      val (same1, same2, nonOverlappingZThis, nonOverlappingZC) = candidateZThis.split(candidateZ, Z)
      (same2,
       nonOverlappingXThis ++ nonOverlappingYThis ++ nonOverlappingZThis,
       nonOverlappingXC ++ nonOverlappingYC ++ nonOverlappingZC)


  // Regex to parse: "on x=10..12,y=10..12,z=10..12"
  val ParsedLine = """(\w+) x=([0-9-]+)..([0-9-]+),y=([0-9-]+)..([0-9-]+),z=([0-9-]+)..([0-9-]+)""".r
  def parseInput(input: Iterator[String]) = 
    val cubes = 
      for ParsedLine(onOff, xMin, xMax, yMin, yMax, zMin, zMax) <- input yield
        Cube(onOff == "on", xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt, zMin.toInt, zMax.toInt)
    cubes.toSeq

  def part1(input: Seq[Cube]) = 
    println(input.length)
    val reactor = Array.ofDim[Boolean](101,101,101)
    input.iterator.filter( c => 
      c.xMin >= -50 && c.xMax <= 50 &&
      c.yMin >= -50 && c.yMax <= 50 &&
      c.zMin >= -50 && c.zMax <= 50)
    .foreach { c => 
      for x <- c.xMin to c.xMax
          y <- c.yMin to c.yMax
          z <- c.zMin to c.zMax do
        reactor(x + 50)(y + 50)(z + 50) = c.on
      }

    var count = 0
    for x <- 0 to 100
        y <- 0 to 100
        z <- 0 to 100 do
          if reactor(x)(y)(z) then count += 1
    count


  def part2(input: ParsedInput) =
    // Split all input cubes into non-overlapping cubes.
    var inputCubes = input.toList
    
    // Invariant, reactor only contains cubes that are turned on.
    var reactor = mutable.Set[Cube]()
    while inputCubes.nonEmpty do
      val nextCube = inputCubes.head
      inputCubes = inputCubes.tail
      reactor.find(_.overlaps(nextCube)) match
        case Some(rcube) =>
          val (overlap, nonOverlappingExisting, nonOverlappingNew) = rcube.split(nextCube)
          if nextCube.off then
            // nextCube is off, replace cube with non overlapping ones.
            reactor -= rcube
//            println(s"Removing $rcube (on)")
            val overlapSize = overlap.size
            val nonOverlappingExistingSize = nonOverlappingExisting.map(_.size).sum
            val rcubeSize = rcube.size
            // TODO - Assert fails with nextCube is within rcube.
            // assert {
            //   overlapSize + nonOverlappingExistingSize == rcubeSize
            // }

            reactor ++= nonOverlappingExisting
            // nonOverlappingExisting.foreach(x =>
            //   println(s"Adding $x (on) ${x.size}")
            // )
          // Add the non overlapping new smaller cubes to check for further conflicts.
          inputCubes = nonOverlappingNew.toList ::: inputCubes
        case None if nextCube.on => 
          reactor += nextCube
  //        println(s"Adding $nextCube (on) ${nextCube.size}")
        case _ =>

  //  println("Reactor:")
  //  reactor.foreach { x => println(s"$x, size=${x.size}") }

    reactor.iterator.map(_.size).sum
