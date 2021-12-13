package aoc

import scala.collection.mutable
import javax.xml.stream.events.EndElement
import scala.annotation.tailrec

object Ex12 extends Exercise:
  type ParsedInput = Map[String, Cave]

  case class Cave(name: String):
    var exits: Seq[Cave] = Seq()
    val isSmall = name.forall(_.isLower)
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
    val cm = caves.toMap
     //for c <- caves.valuesIterator do
     //  println(s"${c.name}: ${c.exits.toString}")
    cm

  // Not tailrec, can be optimize this into a while loop?
  def paths(path: List[Cave]): Seq[List[Cave]] =
    val cave = path.head
    if cave == End then 
      Seq(path)
    else 
      for exitCave <- cave.exits
          if !exitCave.isSmall || !path.contains(exitCave)
          path <- paths(exitCave:: path) yield path

  def part1(input: ParsedInput) =
    val caves = input
    val ps = paths(List(Start))
      
    //for p <- ps do
    //   println(p.reverse.map(_.name).mkString(" -> "))
    ps.length

  def part2(input: ParsedInput) =
    ""