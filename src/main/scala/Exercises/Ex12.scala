package aoc

import scala.collection.mutable
import javax.xml.stream.events.EndElement
import scala.annotation.tailrec

import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{ Success, Failure }

object Ex12 extends Exercise:
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

  // Not tailrec, slightly slower (25-30%) than the tailrec version.
  def paths(path: List[Cave]): Seq[List[Cave]] =
    val cave = path.head
    if cave == End then Seq(path)
    else  
      for exitCave <- cave.exits
          if !exitCave.isSmall || !path.contains(exitCave)
          path <- paths(exitCave :: path) yield path
  
  // Allow small caves to be visited twice.
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


  def part1(input: ParsedInput) =
    paths(List(Start)).length
 
  def part2(input: ParsedInput) =
    paths2(List(Start), false)
