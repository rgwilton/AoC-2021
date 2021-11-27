package aoc

import scala.annotation.tailrec
import scala.collection.mutable
import javax.sound.midi.Instrument

object Ex3 extends Exercise:
  type ParsedInput = Array[Int]

  inline val Add = 1
  inline val Mul = 2
  inline val Halt = 99

  def parseInput(input: Iterator[String]) = input.next.split(",").asIntegers.toArray

  def runProgram(input: Array[Int], arg1: Int, arg2: Int) = 
    val code = input.clone
    code(1) = arg1; code(2) = arg2

    var pc = 0; var running = true
    inline def a1 = code(pc + 1)
    inline def a2 = code(pc + 2)
    inline def tgt = code(pc + 3)
    
    while running do
      code(pc) match 
          case Add => code(tgt) = code(a1) + code(a2); pc += 4
          case Mul => code(tgt) = code(a1) * code(a2); pc += 4
          case Halt => running = false
          case x => throw Exception(s"Unknown opcode $x")

    code(0)

  def part1(input: ParsedInput) = 
    runProgram(input, 12, 2)

  def part2(input: ParsedInput) =
    val r = 
      for 
        n <- 0 to 99
        v <- 0 to 99
        if runProgram(input, n, v) == 19690720
      yield (100 * n + v)        
    r.head

