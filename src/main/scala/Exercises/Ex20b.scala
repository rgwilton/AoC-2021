package aoc

import scala.collection.mutable

object Ex20b extends Exercise:
  override def input = scala.io.Source.fromFile(s"input/input_20.txt").getLines

  type ParsedInput = (Array[Int], Image)
  type Image = Array[Array[Int]]

  def parseInput(input: Iterator[String]) = 
    val algorithm = input.next.map { ch =>
      if ch == '#' then 1 else 0
    }.toArray
 
    input.next()

    var image = 
      (for line <- input yield
        Array(0, 0) ++  
        (for ch <- line yield (if ch == '#' then 1 else 0)) ++
        Array(0, 0)
      ).toArray
    def xMax = image(0).length
    val blankLine = Array.fill(xMax)(0)
    image = Array(blankLine, blankLine) ++ image ++ Array(blankLine, blankLine)
    (algorithm, image.toArray)

  def print(i: Image, xMin: Int, yMin: Int, xMax: Int, yMax: Int) =
    for y <- yMin to yMax do
      val row = 
        for x <- xMin to xMax yield 
          val v = i(y)(x)
          if v == 1 then '#' else '.'
      println(row.mkString(""))
    println()

  def enhance(iterations: Int, algorithm: Array[Int], startImage: Image) =
    var image = startImage

    def iterate(image: Image, default: Int): Image = 
      inline def cell(x: Int, y: Int) = image(y)(x)
      inline def xMax = image(0).length - 1
      inline def yMax = image.length - 1
      val defaultRow = Array.fill(xMax + 3)(default)
      val newImage = 
        Array(defaultRow, defaultRow) ++ 
        (for y <- 1 to yMax - 1 yield
          Array(default, default) ++ 
          (for x <- 1 to xMax - 1 yield
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
            algorithm(temp >> 1)) ++
            Array(default, default)
        ) ++ Array(defaultRow, defaultRow) 
      newImage

    for i <- 1 to iterations/2 do
      if algorithm(0) == 0 then
        image = iterate(image, 0)
        image = iterate(image, 0)
      else
        image = iterate(image, algorithm(0))
        image = iterate(image, algorithm(511))
  
    // Return the count.
    image.map(_.sum).sum 

  def part1(input: (Array[Int], Image)) =
    val (algorithm, startImage) = input
    enhance(2, algorithm, startImage)
    
  def part2(input: (Array[Int], Image)) =
    val (algorithm, startImage) = input
    enhance(50, algorithm, startImage)