package aoc

import scala.collection.mutable

object Ex20 extends Exercise:
  type ParsedInput = (Array[Int], Image, Int, Int)
  type Image = Map[(Int, Int), Int]
  case class StartingPlayer(id: Int, startingPosition: Int)

  val InputRegex = """Player (\d+) starting position: (\d+)""".r
  def parseInput(input: Iterator[String]) = 
    val algorithm = input.next.map { ch =>
      if ch == '#' then 1 else 0
    }.toArray
 
    input.next()

    var xMax = 0
    var yMax = 0
    val image = mutable.Map[(Int, Int), Int]()
      for (line, y) <- input.zipWithIndex do 
        yMax = y
        for (ch, x) <- line.zipWithIndex do
          xMax = x
          image += (x, y) -> (if ch == '#' then 1 else 0)
        
    (algorithm, image.toMap, xMax, yMax)

  def print(i: Image, xMin: Int, yMin: Int, xMax: Int, yMax: Int) =
    for y <- yMin to yMax do
      val row = 
        for x <- xMin to xMax yield 
          val v = i.getOrElse((x, y), 0)
          if v == 1 then '#' else '.'
      println(row.mkString(""))
    println()

  def enhance(iterations: Int, algorithm: Array[Int], startImage: Image, xmax: Int, ymax: Int) =
    var image = startImage
    var xMin = 0
    var yMin = 0
    var xMax = xmax
    var yMax = ymax
    def iterate(default: Int) = 
      def cell(x: Int, y: Int) = image.getOrElse((x, y), default)
      xMin -= 1
      yMin -= 1
      xMax += 1
      yMax += 1
      val newVals = 
        for x <- xMin to xMax
            y <- yMin to yMax yield
          val temp = 
            1 << (9 * cell(x - 1, y - 1)) |
            1 << (8 * cell(x, y - 1)) |
            1 << (7 * cell(x + 1, y - 1)) |
            1 << (6 * cell(x - 1, y)) |
            1 << (5 * cell(x , y)) |
            1 << (4 * cell(x + 1, y)) |
            1 << (3 * cell(x - 1, y + 1)) |
            1 << (2 * cell(x, y + 1)) |
            1 << (1 * cell(x + 1, y + 1))
          (x,y) -> algorithm(temp >> 1)
      image = newVals.toMap
    for i <- 1 to iterations/2 do
      if algorithm(0) == 0 then
        iterate(0)
        iterate(0)
      else
        iterate(algorithm(511))
        iterate(algorithm(0))
  
    // Return the count.
    image.iterator.map(_._2).sum    

  def part1(input: (Array[Int], Image, Int, Int)) =
    val (algorithm, startImage, xm, ym) = input
    enhance(2, algorithm, startImage, xm, ym)
    
  def part2(input: (Array[Int], Image, Int, Int)) =
    val (algorithm, startImage, xm, ym) = input
    enhance(50, algorithm, startImage, xm, ym)