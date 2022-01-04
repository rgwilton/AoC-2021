package aoc

import scala.collection.mutable
import scala.annotation.switch

import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Success
import scala.util.Failure

object Ex22b extends Exercise:
  override def input = scala.io.Source.fromFile(s"input/input_22.txt").getLines

  type ParsedInput = Seq[Cube]
  enum Dimension:
    case X, Y, Z
  import Dimension._

  case class Cube(on: Boolean, xMin: Int, xMax: Int, yMin: Int, yMax: Int, zMin: Int, zMax: Int):
    def off = !on
    def size = 
      if (xMin > xMax || yMin > yMax || zMin > zMax) then 0
      else (1L + xMax - xMin) * (1L + yMax - yMin) * (1L + zMax - zMin)

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

  def calc(input: List[Cube]) =
    var inputCubes = input

    // Store the toplevel bounding cubes as an optimization for initial overlap check.
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
            reactor ++= nonOverlappingExisting
          inputCubes = nonOverlappingNew.toList ::: inputCubes
        case None if nextCube.on => 
          reactor += nextCube
        case _ =>
    //println(reactor.size)
    (reactor.iterator.map(_.size).sum, reactor.size)

  def part2(input: ParsedInput) =
    // Partition cube into two based on the split point 'p' and Dimension 'dim'
    def split(cOpt: Option[Cube], p: Int, dim: Dimension): Seq[Option[Cube]] =
      given Dimension = dim
      cOpt match
        case Some(c) =>
          if c.dMax < p then Seq(Some(c),None)
          else if c.dMin >= p then Seq(None, Some(c))
          else Seq(Some(c.trimDMax(p - 1)), Some(c.trimDMin(p)))
        case None => Seq(None, None)

    // Partition cube into 4 for the given dimension: -50k-, -50k to -1, 0 to 50k-1, 50k+
    def split4(cOpt: Option[Cube], dim: Dimension): Seq[Option[Cube]] = 
      val firstSplit = split(cOpt, 0, dim)
      split(firstSplit(0), -50_000, dim) ++ split(firstSplit(1), 50_000, dim) 


    val splitCubes = 
      for i <- input yield
         for xs <- split4(Some(i), X) 
             xys <- split4(xs, Y)
             xyzs <- split4(xys, Z) yield xyzs

    val soln = 
      Future.sequence {
        for i <- 0 until splitCubes(0).length yield
          Future { 
            val cubes = splitCubes.map(_(i)).flatten
            calc(cubes.toList)
          }
      }
    val res = Await.result(soln, 1 minute)
    val sum = res.map(_._1).sum
    val count = res.map(_._2).sum
    //println(s"Total count = $count")
    sum
   
