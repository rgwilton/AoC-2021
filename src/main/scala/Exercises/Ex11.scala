package aoc

import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayBuilder

object Ex11 extends Exercise:
  type ParsedInput = Array[Array[Int]]

  // ParsedInput is as a 2D array of characters with a boarder of '0' digits (to avoid boundary checks).
  def parseInput(input: Iterator[String]) = 
    val bufIter = input.buffered
    val len = bufIter.head.length
    val zerosRow = Array.ofDim[Int](len + 2)
    def rows = bufIter.map { line => Array(0) ++ line.map(_ - '0').toSeq ++ Seq(0) }
    (ArrayBuffer(zerosRow) ++ rows ++ Seq(zerosRow)).toArray

  def part1(input: ParsedInput) = 
    val grid = input.map(_.clone)

    var flashCount = 0
    var flashes = List[(Int, Int)]()

    def flash(i: Int, j: Int) = 
//      println(s"Flash: $i, $j")
      grid(i)(j) = 0
      flashCount += 1
      flashes :+= (i, j)

    def update(i: Int, j: Int) =
      if (i == -1)
        println("")
      grid(i)(j) match
        case 0 =>
        case 9 => flash(i, j)
        case x => grid(i)(j) = x + 1

    def updateNeighbours(i: Int, j: Int) =
      for x <- -1 to 1 do 
        update(x + i, j + 1)
        update(x + i, j - 1)
      update(i - 1, j)
      update(i + 1, j)

    def processStep(step: Int) =
      for i <- 1 until input.length - 1
          j <- 1 until input(0).length - 1 do
        grid(i)(j) match
          case 9 => flash(i, j)
          case x => grid(i)(j) = x + 1

      while flashes.nonEmpty do
        val f = flashes.head
        flashes = flashes.tail
        updateNeighbours(f._1, f._2)

      // Reset borders.
      for (x <- 0 until input.length) do
        grid(x)(0) = 0
        grid(0)(x) = 0
        grid(x)(input.length - 1) = 0
        grid(input.length - 1)(x) = 0
      
      // println(s"Step: $step")
      // for (x <- 1 until input.length - 1) do
      //   println(grid(x)
      //           .slice(1, grid(x).length -1)
      //           .map(_.toString)
      //           .mkString("  ", "", ""))
      // println("")

    for i <- 1 to 100 do processStep(i)
    flashCount

  // Convert each row into ranges of candidates between 9's.
  def part2(input: ParsedInput) =
    def grid = input

    var flashCount = 0
    var flashes = List[(Int, Int)]()

    def flash(i: Int, j: Int) = 
//      println(s"Flash: $i, $j")
      grid(i)(j) = 0
      flashCount += 1
      flashes :+= (i, j)

    def update(i: Int, j: Int) =
      if (i == -1)
        println("")
      grid(i)(j) match
        case 0 =>
        case 9 => flash(i, j)
        case x => grid(i)(j) = x + 1

    def updateNeighbours(i: Int, j: Int) =
      for x <- -1 to 1 do 
        update(x + i, j + 1)
        update(x + i, j - 1)
      update(i - 1, j)
      update(i + 1, j)

    def processStep(step: Int) =
      for i <- 1 until input.length - 1
          j <- 1 until input(0).length - 1 do
        grid(i)(j) match
          case 9 => flash(i, j)
          case x => grid(i)(j) = x + 1

      while flashes.nonEmpty do
        val f = flashes.head
        flashes = flashes.tail
        updateNeighbours(f._1, f._2)

      // Reset borders.
      for (x <- 0 until input.length) do
        grid(x)(0) = 0
        grid(0)(x) = 0
        grid(x)(input.length - 1) = 0
        grid(input.length - 1)(x) = 0

    def allFlashed = grid.forall { x => x.forall (_ == 0) }
    
    Iterator.from(1).dropWhile { i =>
      processStep(i)
      !allFlashed
    }.next()
