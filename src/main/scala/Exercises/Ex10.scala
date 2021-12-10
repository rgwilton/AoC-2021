package aoc

import scala.collection.mutable

object Ex10 extends Exercise:
  type ParsedInput = Seq[String]

  def parseInput(input: Iterator[String]) = input.toSeq

  def matchingBracket(c: Char) =
    c match
      case '(' => ')'; case '{' => '}'; case '[' => ']'; case '<' => '>'

  def unmatchedInput(input: Seq[String]) =
    for line <- input yield
      var stack = List[Char]()
      val remLine = 
        line.dropWhile { ch =>
          ch match
            case '(' | '{' | '[' | '<' => 
              stack = ch :: stack; true
            case _ if stack.nonEmpty && matchingBracket(stack.head) == ch =>
              stack = stack.tail; true
            case _ => 
              false
          }
      (remLine, stack)

  def part1(input: ParsedInput) = 
    def value(ch: Char) =
        ch match
          case ')' => 3; case ']' => 57; case '}' => 1197; case '>' => 25137

    unmatchedInput(input)
    .collect {
      case (remline, stack) if remline.nonEmpty => remline.head
    }
    .map(value)
    .sum
    
  def part2(input: ParsedInput) =
    def value(ch: Char) =
        ch match
          case '(' => 1 ; case '[' => 2; case '{' => 3; case '<' => 4

    def calcScore(l: List[Char]) =
      l.foldLeft(0L) { (sum, ch) => (5 * sum) + value(ch) }

    unmatchedInput(input)
    .collect {
      case (remline, stack) if remline.length == 0 => stack
    }
    .map(calcScore)
    .median