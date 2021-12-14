package aoc

import scala.collection.mutable
import javax.xml.stream.events.EndElement
import scala.annotation.tailrec

import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{ Success, Failure }

object Ex12b extends Exercise:
  override def input = scala.io.Source.fromFile(s"input/input_12.txt").getLines
  type ParsedInput = Map[String, Cave]
  type CavePath = List[Cave]

  case class Cave(name: String):
    var exits: Seq[Cave] = Seq()
    val isSmall = name.forall(_.isLower)
    var isBig = !isSmall
    val maxCount = if isSmall then 1 else Integer.MAX_VALUE

  final val Start = Cave("start")
  final val End = Cave("end")

  def parseInput(input: Iterator[String]) = 
    val caves = mutable.Map[String, Cave]()
    Start.exits = Seq()
    caves ++= Iterable("start" -> Start, "end" -> End)

    def addCaveLink(src: Cave, dst: Cave) =
      if src != End && dst != Start then src.exits :+= dst

    for line <- input
        Array(a, b) = line.split('-') do
      val ca = caves.getOrElseUpdate(a, Cave(a))
      val cb = caves.getOrElseUpdate(b, Cave(b))
      addCaveLink(ca, cb)
      addCaveLink(cb, ca)
    caves.toMap

  def paths1(path: List[Cave]): Int =
    val cave = path.head
    if cave == End then 1
    else  
      var sum = 0
      for exitCave <- cave.exits
          if !exitCave.isSmall || !path.contains(exitCave)
          x = paths1(exitCave :: path) do
           sum += x
      sum

  def paths1par(path: List[Cave]): Int =
    val cave = path.head
    if cave == End then 1
    else  
      if path.lengthIs == 2 then
        val res =
          Future.sequence{
            cave.exits.filter{ exitCave =>
              !exitCave.isSmall || !path.contains(exitCave)
            }.map { exitCave => Future { paths1(exitCave :: path)} }
          }.map(_.sum)
        Await.result(res, 1 minute)
      else
        (for exitCave <- cave.exits
            if !exitCave.isSmall || !path.contains(exitCave) yield
            paths1par(exitCave :: path)).sum

  def paths2(path: List[Cave], smallTwice: Boolean): Int =
    var count = 0
    for exitCave <- path.head.exits do
      if exitCave.isBig then 
        count += paths2((exitCave :: path), smallTwice)
      else if exitCave == End then
        count += 1
      else if !path.contains(exitCave) then
        count += paths2((exitCave :: path), smallTwice)
      else if !smallTwice && path.count(_ == exitCave) == 1 then
        count += paths2((exitCave :: path), true) 
    count

  def paths2par(path: List[Cave], smallTwice: Boolean): Int =
    if path.lengthIs >= 6 then 
      var count = 0
      for exitCave <- path.head.exits do
        if exitCave.isBig then 
          count += paths2((exitCave :: path), smallTwice)
        else if exitCave == End then
          count += 1
        else if !path.contains(exitCave) then
          count += paths2((exitCave :: path), smallTwice)
        else if !smallTwice && path.count(_ == exitCave) == 1 then
          count += paths2((exitCave :: path), true) 
      count
    else
      val exits = path.head.exits
      val res = 
        Future.sequence{
          //val res = 
          for exitCave <- exits yield
            if exitCave.isBig then 
              Future { paths2par((exitCave :: path), smallTwice) }
            else if exitCave == End then
              Future.successful(1)
            else if !path.contains(exitCave) then
              Future { paths2par((exitCave :: path), smallTwice) }
            else if !smallTwice && path.count(_ == exitCave) == 1 then
              Future { paths2par((exitCave :: path), true) }
            else
              Future.successful(0)
        }.map(_.sum)
      Await.result(res, 1 minute)

  def part1(input: ParsedInput) =
    paths1par(List(Start))

  def part2(input: ParsedInput) =
    paths2par(List(Start), false)