package aoc

import scala.collection.mutable

object Ex8 extends Exercise:
  type DigitSegments = Set[Char]
  type UnknownDigits = Set[DigitSegments]
  type KnownDigits = Map[Int, DigitSegments]

  case class Line(signals: Seq[DigitSegments], outputs: Seq[DigitSegments])
  type ParsedInput = Seq[Line]

  def parseInput(input: Iterator[String]) = 
    val lines = 
      for line <- input yield
        val Array(signalText, outputText) = line.split('|')
        val signals = signalText.split(' ')
        val outputs = outputText.strip.nn.split(' ')
        Line(signals.map(_.toSet), outputs.map(_.toSet))
    lines.toSeq

  // Segement candidates:
  // Each array element holds the set of possible segments
  // 0 represents what  

  val find1 = (unknown: UnknownDigits, known: KnownDigits) =>
    1 -> unknown.find { _.size == 2 }.get

  val find7 = (unknown: UnknownDigits, known: KnownDigits) =>
    7 -> unknown.find { _.size == 3 }.get

  val find4 = (unknown: UnknownDigits, known: KnownDigits) =>
    4 -> unknown.find { _.size == 4 }.get

  val find8 = (unknown: UnknownDigits, known: KnownDigits) =>
    8 -> unknown.find { _.size == 7 }.get

  val find3 = (unknown: UnknownDigits, known: KnownDigits) =>
    // Len = 5 && contains 1's segments
    3 -> unknown.find { segs => segs.size == 5 && (segs & known(1)) == known(1) }.get

  val find9 = (unknown: UnknownDigits, known: KnownDigits) =>
    // Len = 6 && contains 3's segments
    9 -> unknown.find { segs => segs.size == 6 && (segs & known(3)) == known(3) }.get

  val find0 = (unknown: UnknownDigits, known: KnownDigits) =>
    // Len = 6 && contains 7's segments
    0 -> unknown.find { segs => segs.size == 6 && (segs & known(7)) == known(7) }.get

  val find6 = (unknown: UnknownDigits, known: KnownDigits) =>
    6 -> unknown.find { _.size == 6 }.get

  val find5 = (unknown: UnknownDigits, known: KnownDigits) =>
    // if x + 1's digits == 9 then 5
    //known.foreach((i, s) => println(s" $i: $s"))
    //println(known(1))
    //println(known(9))
    //println(unknown.head)
    //println(unknown.tail)
    5 -> unknown.find { segs => 
      val unknownCount = unknown.size
      val known9segs = known(9)
      val combinedSegs = segs | known(1)
      combinedSegs == known(9) }.get

  val find2 = (unknown: UnknownDigits, known: KnownDigits) =>
    2 -> unknown.head

  val findFuncs = List(find1, find7, find4, find8, find3, find9, find0, find6, find5, find2)

  def findDigits(unknown: UnknownDigits): KnownDigits =
    var unknownSegs = unknown
    var knownSegs: KnownDigits = Map()
    var fns = findFuncs
    while (fns.nonEmpty) do
      val found = fns.head(unknownSegs, knownSegs)
      knownSegs += found
      unknownSegs -= found._2
      fns = fns.tail
    knownSegs



  def part1(input: ParsedInput) = 
    def knownDigit(s: Set[Char]) = s.size == 2 || s.size == 3 || s.size == 4 || s.size == 7
    input.map(_._2.count(knownDigit)).sum

  def part2(input: ParsedInput) =
    def calcValue(line: Line) =
      val digitsMap = findDigits(line.signals.toSet)
      // digitsMap.foreach((i, s) => println(s" $i: $s"))
      // println()
      // line.outputs.foreach(println(_))
      line.outputs.map(segs => digitsMap.find( (d, s) => s == segs).get._1.toString).mkString.toInt
    input.map(calcValue).sum
