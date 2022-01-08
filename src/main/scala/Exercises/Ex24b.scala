package aoc

import scala.collection.mutable

object Ex24b extends Exercise:
  override def input = scala.io.Source.fromFile(s"input/input_24.txt").getLines
  type ParsedInput = Seq[Instruction]

  enum Reg:
    case W, X, Y, Z

  sealed trait Instruction
  case class Inp(a: Reg) extends Instruction
  case class Add(a: Reg, b: Reg | Int) extends Instruction
  case class Mul(a: Reg, b: Reg | Int) extends Instruction
  case class Div(a: Reg, b: Reg | Int) extends Instruction
  case class Mod(a: Reg, b: Reg | Int) extends Instruction
  case class Eql(a: Reg, b: Reg | Int) extends Instruction

  type Program = Seq[Instruction]

  def eval(program: Program, input: Iterator[Int], regs: Array[Long]) =
    def src(ri: Reg|Int): Long = 
      ri match
        case r:Reg => regs(r.ordinal)
        case i:Int => i
    for inst <- program do
      inst match
        case Add(a, b) => regs(a.ordinal) += src(b)
        case Mul(a, b) => regs(a.ordinal) *= src(b)
        case Div(a, b) => regs(a.ordinal) /= src(b)
        case Mod(a, b) => regs(a.ordinal) %= src(b)
        case Eql(a, b) => 
          regs(a.ordinal) = if src(a) == src(b) then 1 else 0
        case Inp(a) => regs(a.ordinal) = input.next

  def parseInput(input: Iterator[String]) = 
    def reg(s: String) = Reg.valueOf(s.toUpperCase.nn)
    def regOrInt(s: String): Reg | Int = 
      try s.toInt catch case e:Exception => reg(s)

    val instructions = 
      for line <- input yield
        line.split(' ').toSeq match
          case Seq("inp", a) => Inp(reg(a))
          case Seq("add", a, b) => Add(reg(a), regOrInt(b))
          case Seq("mul", a, b) => Mul(reg(a), regOrInt(b))
          case Seq("div", a, b) => Div(reg(a), regOrInt(b))
          case Seq("mod", a, b) => Mod(reg(a), regOrInt(b))
          case Seq("eql", a, b) => Eql(reg(a), regOrInt(b))
          case _ => throw new Exception(s"Unrecognised instruction '$line'")
    instructions.toSeq

  // Split the program into blocks and calc the max value for each block.
  case class PowerBlock(block: Program, maxOutput: Long)
  def getBlocks(p: Program) =
    val revBlocks = p.grouped(18).toSeq.reverse
    var power = 0
    val powerBlocks = 
      for block <- revBlocks yield
        val reduce = block.contains(Div(Reg.Z, 26))
        val maxOutput = Math.pow(26, power).toLong
        if reduce then power += 1 else power -= 1
        PowerBlock(block, maxOutput)
    powerBlocks.reverse.toList

  def part1(input: Seq[Instruction]) =
    val regs = Array[Long](0, 0, 0, 0)
    val seen = mutable.Set[(Int, Long, Int)]()
    def calcMax(depth: Int, pbs: List[PowerBlock], z: Long, candidates: List[Int]): Option[List[Int]] =
      if pbs.isEmpty then
        if z == 0 then Some(candidates) else None
      else
        val pb = pbs.head
        val candidate = 9 to (1, step = -1) 
        candidate.view.flatMap { c =>
          regs(3) = z
          eval(pb.block, Iterator(c), regs)
          if regs(3) < pb.maxOutput && !seen.contains((depth,z,c)) then
              seen += ((depth,z,c))
              calcMax(depth + 1, pbs.tail, regs(3), c :: candidates)
          else None
        }.headOption
    val blocks = getBlocks(input)
    val res = calcMax(0, blocks, 0, List())
    res.get.reverse.mkString("")

  def part2(input: ParsedInput) =
    val regs = Array[Long](0, 0, 0, 0)
    def calcMin(pbs: List[PowerBlock], z: Long, candidates: List[Int]): Option[List[Int]] =
      if pbs.isEmpty then
        if z == 0 then Some(candidates) else None
      else
        val pb = pbs.head
        val candidate = 1 to 9 
        candidate.view.flatMap { c =>
          regs(3) = z
          eval(pb.block, Iterator(c), regs)
          if regs(3) < pb.maxOutput then
            calcMin(pbs.tail, regs(3), c :: candidates)
          else None
        }.headOption
    val blocks = getBlocks(input)
    val res = calcMin(blocks, 0, List())
    res.get.reverse.mkString("")
