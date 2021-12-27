package aoc

import scala.collection.mutable

object Ex25 extends Exercise:
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
    var changed = true
    var iterations = 0

    def display = 
      for j <- 0 until yMax do
        val l =
          array(j).map(x =>
            x match
              case Horizontal => ">"
              case Vertical => "v"
              case _ => "."
          ).mkString("")
        println(l)
      println()

    while (changed) do
      var hChanges = List[(Int, Int)]()
      var vChanges = List[(Int, Int)]()
      var nChanges = List[(Int, Int)]()

      // Improve performance by considering x and y in the same loop?
      for y <- 0 until yMax
          x <- 0 until xMax do
        if array(y)(x) == None then 
          val prevX = (x - 1 + xMax) % xMax
          val prevY = (y - 1 + yMax) % yMax
          if array(y)(prevX) == Horizontal then
            hChanges ::= (x, y)
            if array(prevY)(prevX) == Vertical then
              vChanges ::= (prevX, y)
              nChanges ::= (prevX, prevY)
            else
              nChanges ::= (prevX, y)
          else if array(prevY)(x) == Vertical then
            vChanges ::= (x, y)
            nChanges ::= (x, prevY)

      for (x, y) <- nChanges do array(y)(x) = None
      for (x, y) <- hChanges do array(y)(x) = Horizontal
      for (x, y) <- vChanges do array(y)(x) = Vertical
      changed = nChanges.nonEmpty
      iterations += 1

    iterations

  def part2(input: ParsedInput) =
    ""