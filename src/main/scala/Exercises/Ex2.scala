package aoc

import scala.annotation.tailrec

object Ex2 extends Exercise:
  type ParsedInput = Seq[Command]

  case class Command(move: Move, distance: Int)

  enum Move:
    case Forward, Up, Down
  import Move._

  def parseInput(input: Iterator[String]) = 
    val LineR = """(\w+) (\d+)""".r
    input.collect { 
      case LineR(dir, value) => Command(Move.valueOf(dir.capitalize), value.toInt)
      case x => throw new Exception(s"'$x' doessn't match")
    }.toSeq

  def part1(input: ParsedInput) =
    var horizontal, depth = 0
    for Command(move, x) <- input do
      move match
        case Up => depth -= x
        case Down => depth += x
        case Forward => horizontal += x
    horizontal * depth

  def part2(input: ParsedInput) =
    var horizontal, depth, aim = 0
    for Command(move, x) <- input do
      move match
        case Up => aim -= x
        case Down => aim += x
        case Forward => horizontal += x; depth += aim * x
    horizontal * depth
