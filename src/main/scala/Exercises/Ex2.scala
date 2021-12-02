package aoc

import scala.annotation.tailrec

object Ex2 extends Exercise:
  type ParsedInput = Seq[Command]

  enum Move:
    case Forward, Up, Down
  
  object Command:
    def unapply(input: String): Command =
      val Array(direction, value) = input.split(' ')
      Command(Move.valueOf(direction.capitalize), value.toInt)
      
  case class Command(move: Move, distance: Int)

  def parseInput(input: Iterator[String]) = input.map(Command.unapply).toSeq

  def part1(input: ParsedInput) =
    var horizontal, depth = 0
    for Command(move, x) <- input do
      move match
        case Move.Up => depth -= x
        case Move.Down => depth += x
        case Move.Forward => horizontal += x
    horizontal * depth

  def part2(input: ParsedInput) =
    var horizontal, depth, aim = 0
    for Command(move, x) <- input do
      move match
        case Move.Up => aim -= x
        case Move.Down => aim += x
        case Move.Forward => horizontal += x; depth += aim * x
    horizontal * depth
