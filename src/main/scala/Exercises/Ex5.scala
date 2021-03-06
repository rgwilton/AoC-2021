package aoc

object Ex5 extends Exercise:
  type ParsedInput = Seq[Line]

  case class Line(x1: Int, y1: Int, x2: Int, y2: Int):
    inline def isHorizontal = y1 == y2
    inline def isVertical = x1 == x2
    inline def isDiagonal = !(isHorizontal || isVertical)

  case class Map(xMax: Int, yMax: Int):
    val map = Array.ofDim[Int](xMax, yMax)

    inline def range(m: Int, n: Int) = 
      if m < n then m to n else m to (n, step = -1)

    def drawLine(l: Line) =
      if l.isHorizontal then
        for x <- range(l.x1, l.x2) do
          map(x)(l.y1) += 1
      else if l.isVertical then
        for y <- range(l.y1, l.y2) do
          map(l.x1)(y) += 1
      else
        val xRange = range(l.x1, l.x2)
        val yRange = range(l.y1, l.y2)
        for (x, y) <- xRange.zip(yRange) do
          map(x)(y) += 1

    def count =
      var c = 0
      for x <- 0 until xMax
          y <- 0 until yMax do
        if map(x)(y) > 1 then c += 1
      c
    
    def print() =
      for y <- 0 until yMax do
        println((for x <- 0 until xMax yield map(x)(y).toString).mkString(""))

  final val lineR = raw"(\d+),(\d+) -> (\d+),(\d+)".r
  def parseInput(input: Iterator[String]) = 
    for lineR(x1, y1, x2, y2) <- input.toSeq yield 
      Line(x1.toInt, y1.toInt, x2.toInt, y2.toInt)

  def part1(input: ParsedInput) = 
    val m = Map(1000, 1000)
    input.filterNot(_.isDiagonal).foreach(m.drawLine)
    m.count

  def part2(input: ParsedInput) =
    val m = Map(1000, 1000)
    input.foreach(m.drawLine)
    m.count