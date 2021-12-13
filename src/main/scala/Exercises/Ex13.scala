package aoc

import scala.collection.mutable

object Ex13 extends Exercise:
  type Fold = (Char, Int)
  type Paper = mutable.Set[(Int, Int)]
  type ParsedInput = (Paper, Seq[Fold])

  val FoldR = """fold along (\w+)=(\d+)""".r
  def parseInput(input: Iterator[String]) = 
    val paper = mutable.Set[(Int, Int)]()
    var folds = Seq[(Char, Int)]()
    for line <- input do
      if line.contains(',') then
        val Array(x,y) = line.split(',') 
        paper.add((x.toInt, y.toInt))
      else if line.startsWith("fold") then
        for FoldR(direction, position) <- Seq(line) do
          folds :+= (direction(0), position.toInt)
    (paper, folds)
    
  def foldPaper(paper: Paper, fold: Fold) =
    val (dir, pos) = fold
    for point@(px, py) <- paper do
      dir match
        case 'x' if px > pos =>
          val newX = pos - (px - pos)
          paper.remove(point)
          paper.add((newX, py))
        case 'y' if py > pos =>
          val newY = pos - (py - pos)
          paper.remove(point)
          paper.add((px, newY))
        case _ => // No change required.

  def printPaper(paper: Paper) =
    val maxX = paper.map(_._1).max
    val maxY = paper.map(_._2).max
    for y <- 0 to maxY do
      for x <- 0 to maxX do
        print(if paper.contains((x, y)) then "#" else " ")
      println("")
    println("\n")

  def part1(input: ParsedInput) =
    val (paper, folds) = input
    foldPaper(paper, folds.head)
    paper.size

  def part2(input: ParsedInput) =
    val (paper, folds) = input
    for fold <- folds.tail do
      foldPaper(paper, fold)
    //printPaper(paper)
    paper.size
    