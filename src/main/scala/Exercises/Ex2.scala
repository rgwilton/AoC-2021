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
    val cmdSums = input.groupMapReduce(_.move)(_.distance)(_ + _)
    (cmdSums(Move.Down) - cmdSums(Move.Up)) * cmdSums(Move.Forward)

  def part2(input: ParsedInput) =
    var horizontal, depth, aim = 0
    for Command(move, x) <- input do
      move match
        case Move.Up => aim -= x
        case Move.Down => aim += x
        case Move.Forward => horizontal += x; depth += aim * x
    horizontal * depth
