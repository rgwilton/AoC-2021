package aoc

import scala.collection.mutable

object Ex25b extends Exercise:
  override def input = scala.io.Source.fromFile(s"input/input_25.txt").getLines

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

    while (changed) do
      changed = false

      // Slightly improve performance by splitting horizonal from vertical processing
      // and update at the same time.
      for y <- 0 until yMax do
        val x0 = array(y)(0)
        var nextXval = x0
        for x <- 0 until xMax - 1 do
          val xVal = nextXval
          nextXval = array(y)(x + 1)
          if xVal == Horizontal && nextXval == None then
            array(y)(x) = None
            array(y)(x + 1) = Horizontal
            changed = true
        val xLast = nextXval
        if xLast == Horizontal && x0 == None then
          array(y)(0) = Horizontal
          array(y)(xMax - 1) = None
          changed = true

      for x <- 0 until xMax do
        val y0 = array(0)(x)
        var nextYval = y0
        for y <- 0 until yMax - 1 do
          val yVal = nextYval
          nextYval = array(y + 1)(x)
          if yVal == Vertical && nextYval == None then
            array(y)(x) = None
            array(y + 1)(x) = Vertical
            changed = true
        val yLast = nextYval
        if yLast == Vertical && y0 == None then
          array(0)(x) = Vertical
          array(yMax - 1)(x) = None
          changed = true

      iterations += 1

    iterations

  def part2(input: ParsedInput) =
    ""