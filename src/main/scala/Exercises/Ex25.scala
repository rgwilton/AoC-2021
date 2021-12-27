package aoc

import scala.collection.mutable

object Ex25 extends Exercise:
  //type EntryMap = mutable.Map[(Int, Int), Direction]
  type ParsedInput = Array[Array[Direction]]

  enum Direction:
    case Horizontal, Vertical, None
  import Direction._

  def parseInput(input: Iterator[String]) =
    for (line, y) <- input.toArray.zipWithIndex yield
      for (char, x) <- line.toArray.zipWithIndex yield
        char match 
          case '>' => Horizontal
          case 'v' => Vertical
          case '.' => None

  def part1(input: ParsedInput) =
    var array = input
    val xMax = input(0).length
    val yMax = input.length
    println(s"Total = ${xMax * yMax}")

    // Move > first.
    var changed = true
    var changeCount = 0
    var iterations = 0
    while (changed) do
      changed = false
      // for j <- 0 until yMax do
      //   val l =
      //     array(j).map(x =>
      //       x match
      //         case Horizontal => ">"
      //         case Vertical => "v"
      //         case _ => "."
      //     ).mkString("")
      //   println(l)
      // println()

      var nextArray = 
        (for j <- 0 until yMax yield
          (for i <- 0 until xMax yield
            inline def nextX = (i + 1) % (xMax)
            inline def prevX = (i - 1 + xMax) % (xMax)
            if array(j)(i) == Horizontal && array(j)(nextX) == None then
              changed = true
              changeCount += 1
              None
            else if array(j)(i) == None && array(j)(prevX) == Horizontal then 
              Horizontal
            else
              array(j)(i)
          ).toArray
        ).toArray
      array = nextArray

      nextArray = 
        (for j <- 0 until yMax yield
          (for i <- 0 until xMax yield
            inline def nextY = (j + 1) % (yMax)
            inline def prevY = (j - 1 + yMax) % (yMax)
            if array(j)(i) == Vertical && array(nextY)(i) == None then
              changed = true
              changeCount += 1
              None
            else if array(j)(i) == None && array(prevY)(i) == Vertical then 
              Vertical
            else
              array(j)(i)
          ).toArray
        ).toArray
      array = nextArray
            
      iterations += 1

    println(changeCount)
    println(s"processed cells = ${xMax * yMax * iterations}")
    iterations
  
  def part2(input: ParsedInput) =
    ""