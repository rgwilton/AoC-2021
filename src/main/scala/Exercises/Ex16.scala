package aoc

import scala.collection.mutable
import _root_.aoc.Ex16.Packet.Op

object Ex16 extends Exercise:
  type ParsedInput = Iterator[Byte]

  def parseInput(input: Iterator[String]) = 
    input.next.iterator.grouped(2).map { c =>
      Integer.parseInt(c.mkString, 16).toByte
    }

  // Allows integers of variable numbers of bits to be pulled from the Iterator.
  class BitIterator(input: Iterator[Byte]) extends Iterator[Long]:
    var bitsConsumed = 0
    var curBits = 0
    var curValue = 0L

    def getLong(bits: Int): Long = 
      while curBits < bits do
        curBits += 8        
        curValue = (curValue << 8) | (input.next &0xFF)
      val retVal = curValue  >> (curBits - bits)
      curBits = curBits - bits
      def mask = (1 << curBits) - 1
      curValue = curValue & mask
      bitsConsumed += bits
      retVal

    def getInt(bits: Int): Int = getLong(bits).toInt
    def hasNext = input.hasNext
    def next() = getLong(1)

  object Packet:
    enum LenTypeId:
      case Length(bits: Int)
      case Count(count: Int)

    enum Op:
      case Sum, Product, Minimum, Maximum, Literal, GreaterThan, LessThan, Equal

    // Returns the packet and number of bits consumed during parsing.
    def parse(bits: BitIterator): Packet =
      def bitsConsumed = bits.bitsConsumed

      def getVersion = bits.getInt(3)
      def getTypeID = Op.fromOrdinal(bits.getInt(3))
      def getLiteral = 
        var nextBits = bits.getLong(5)
        var value = nextBits & 0xF
        while (nextBits & 0x10) != 0 do
          nextBits = bits.getLong(5)
          value = (value << 4) | (nextBits & 0xF)
        value
      def getLength =
        if bits.getLong(1) == 0 then
          LenTypeId.Length(bits.getInt(15))
        else
          LenTypeId.Count(bits.getInt(11))

      val ver = getVersion
      val typeId = getTypeID
      if typeId == Op.Literal then
        Literal(ver, getLiteral)
      else
        // All other packets are operators.
        getLength match
          case LenTypeId.Length(len) =>
            val maxBits = bitsConsumed + len
            var subPkts = List[Packet]()
            while bitsConsumed < maxBits do
              subPkts ::= parse(bits)
            Operator(ver, typeId, subPkts.reverse)
            
          case LenTypeId.Count(count) =>
            val c = count
            val subPkts = for x <- 0 until c yield parse(bits)
            Operator(ver, typeId, subPkts)

  trait Packet:
    def version: Int
    def operator: Op
    def subPackets: Seq[Packet]
    def eval: Long

  case class Literal(version: Int, literalValue: Long) extends Packet:
    val operator = Op.Literal
    val subPackets = Seq()
    def eval = literalValue

  case class Operator(version: Int, operator: Op, subPackets: Seq[Packet]) extends Packet:
    def eval: Long =
      (operator, subPackets) match
        case (Op.Sum, pkts) => pkts.map(_.eval).sum
        case (Op.Product, pkts) => pkts.map(_.eval).product
        case (Op.Minimum, pkts) => pkts.map(_.eval).min
        case (Op.Maximum, pkts) => pkts.map(_.eval).max
        case (Op.GreaterThan, Seq(pkt1, pkt2)) => if pkt1.eval > pkt2.eval then 1 else 0
        case (Op.LessThan, Seq(pkt1, pkt2)) => if pkt1.eval < pkt2.eval then 1 else 0
        case (Op.Equal, Seq(pkt1, pkt2)) => if pkt1.eval == pkt2.eval then 1 else 0
        case _ => throw Exception("Cannot evaluate")

  var packet:Option[Packet] = None
  def part1(input: ParsedInput) = 
    val pkt = Packet.parse(BitIterator(input))
    packet = Option(pkt)

    def countVersions(p: Packet): Int =
      p.version + p.subPackets.map(countVersions).sum

    countVersions(pkt)

  def part2(input: ParsedInput) =
    packet.get.eval