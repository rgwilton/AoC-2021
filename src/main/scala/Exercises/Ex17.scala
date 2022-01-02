package aoc

import scala.collection.mutable

object Ex17 extends Exercise:
  type ParsedInput = Target

  case class Target(xMin: Int, xMax: Int, yMin: Int, yMax: Int):
    def isInTarget(x: Int, y: Int) = 
      xMin <= x && x <= xMax && yMin <= y && y <= yMax

  val InputR = """target area: x=([\d-]+)\.\.([\d-]+), y=([\d-]+)\.\.([\d-]+)""".r
  def parseInput(input: Iterator[String]) = 
    (for InputR(xMin, xMax, yMin, yMax) <- input yield
      Target(xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt)).next

  def positions(xVel: Int, yVel: Int)(using input: Target): Iterator[(Int, Int)] =
    var xv = xVel
    var yv = yVel

    def step(x: Int, y: Int): (Int, Int) =
      val newX = x + xv
      val newY = y + yv
      if (xv > 0) xv -= 1
      yv -= 1
      (newX, newY)

    Iterator
    .iterate((0, 0))(step)
    .takeWhile{ (x, y) => x <= input.xMax && y >= input.yMin }

  var part1ValidPaths = 0

  def part1(input: Target) = 
    // Find candidate x values.
    given Target = input
    part1ValidPaths = 0
    val xVelocityMin = ((-1 + Math.sqrt(1 + 8 * input.xMin))/2).round.toInt
    val xVelocityMax = input.xMax/2
    val yVelocityMax = 0 - input.yMin
    
    def max(allPositions: Iterator[(Int, Int)]): Option[Int] =
      var isValid = false
      var maxY = Integer.MIN_VALUE
      for (x, y) <- allPositions do
        maxY = maxY max y
        isValid |= input.isInTarget(x, y)
      if isValid then Some(maxY) else None

    var highestMaxY = Integer.MIN_VALUE
    var minY = 1
    for x <- xVelocityMin to xVelocityMax
        y <- 1 to yVelocityMax
        p = positions(x, y)
        maxY <- max(p) do
      part1ValidPaths += 1
      if maxY > highestMaxY then
        highestMaxY = maxY
        minY = y

    highestMaxY

  def part2(input: ParsedInput) =
    // Consider all of the part1 paths.
    // Add in ones that go directly into the target area as their first position.
    // Also consider paths that go into the target area with at least one intermediate position.
    given Target = input
    val xVelocityMin = ((-1 + Math.sqrt(1 + 8 * input.xMin))/2).round.toInt
    val xVelocityMax = 1 + input.xMax/2
    val yVelocityMax = 0
    val yVelocityMin = -1 + input.yMin/2
    
    def isValid(allPositions: Iterator[(Int, Int)]): Boolean =
      allPositions.exists((x,y) => input.isInTarget(x, y))

    var highestMaxY = Integer.MIN_VALUE
    var minY = 1
    val targetSize = (1 + input.yMax - input.yMin) * (1 + input.xMax - input.xMin) 
    var count = 0
    for x <- xVelocityMin to xVelocityMax
        y <- yVelocityMin to yVelocityMax
        p = positions(x, y)
        if isValid(p) do
      count += 1

    count + part1ValidPaths + targetSize