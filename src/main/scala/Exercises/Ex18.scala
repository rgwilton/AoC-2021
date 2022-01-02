package aoc

import scala.collection.mutable
import scala.annotation.tailrec

object Ex18 extends Exercise:
  type ParsedInput = Seq[Pair]

  object Pair:
    def parse(s: String): Pair | Int =
      val chars = s.iterator
      def p(): Pair | Int = 
        chars.next match
          case '[' =>
            val firstPair = p()
            require(chars.next == ',')
            val secondPair = p()
            require(chars.next == ']')
            Pair(firstPair, secondPair)
          case digit =>
            (digit - '0').toInt
      p()

  case class Pair(var left: Pair | Int, var right: Pair | Int):
    def +(p: Pair | Int) = Pair(this, p)

    override def toString: String = 
      s"[${left.toString},${right.toString}]"

    // Split the first left most element found, or otherwise return the unchanged element.
    def split: Pair = 
      def s(x: Pair|Int) = 
        x match
          case p: Pair => p.split
          case i: Int =>
            if i >= 10 then
              Pair(i / 2, (i + 1) / 2)
            else
              x

      // Try left branch first, or otherwise the right branch is no split was found.
      val newLeft = s(left)
      if newLeft.equals(left) then
        val newRight = s(right)
        if newRight.equals(right) then
          this
        else
          Pair(left, newRight)
      else
        Pair(newLeft, right)

    def addLeft(frag: Int): Pair =
      val newLeft: Pair | Int =
        left match 
          case p:Pair => p.addLeft(frag)
          case i:Int => i + frag
      Pair(newLeft, right)

    def addRight(frag: Int): Pair =
      val newRight: Pair | Int =
        right match 
          case p:Pair => p.addRight(frag)
          case i:Int => i + frag
      Pair(left, newRight)

    extension (pi: Pair | Int)
      def addLeft(frag: Int): Pair | Int =
        pi match
          case p:Pair => p.addLeft(frag)
          case i:Int => i + frag 

      def addRight(frag: Int): Pair | Int =
        pi match
          case p:Pair => p.addRight(frag)
          case i:Int => i + frag 

    final def explode(depth: Int): (Pair, Option[Either[Int, Int]]) =
      if depth == 4 then
        (left, right) match
          case (Pair(x:Int, y:Int), _) =>
            (Pair(0, right.addLeft(y)), Some(Left(x)))
          case (x: Int, Pair(y:Int, z:Int)) =>
            (Pair(x + y, 0), Some(Right(z)))
          case (x: Int, y: Int) => (this, None)
          case _ => 
            throw new Exception("Impossible.")
      else
        val (newLeft, frag) = 
          left match
            case p:Pair => p.explode(depth + 1)
            case i: Int => (left, None)
        inline def leftExploded = !newLeft.equals(left)
        if leftExploded then
          frag match 
            case Some(Right(frag)) => 
              (Pair(newLeft, right.addLeft(frag)), None)
            case _ => 
              (Pair(newLeft, right), frag)
        else
          right match
            case p:Pair => 
              val (newRight, frag) = p.explode(depth + 1)
              inline def rightExploded = !newRight.equals(right)
              if rightExploded then 
                frag match 
                case Some(Left(frag)) => 
                  (Pair(left.addRight(frag), newRight), None)
                case _ => 
                  (Pair(left, newRight), frag)     
              else 
                (this, None)
            case i: Int => (this, None)

    def explode: Pair = 
      val (newPair, frag) = explode(1)
      newPair

    @tailrec
    final def reduce: Pair = 
      val explodedPair = this.explode
      if this.equals(explodedPair) then
        val splitPair = this.split
        if this.equals(splitPair) then
          // Reduced.
          this
        else
          splitPair.reduce
      else
        explodedPair.reduce

    final def magnitude: Long = 
      val leftMag = 
        left match
          case p:Pair => p.magnitude
          case i:Int => i.toLong
      val rightMag = 
        right match
           case p:Pair => p.magnitude
           case i:Int => i.toLong

      (3 * leftMag) + (2 * rightMag)
         
  def parseInput(input: Iterator[String]) = 
    input.map(Pair.parse).collect{ 
      case p:Pair => p
    }.toSeq

  def part1(input: ParsedInput) = 
    input
    .tail
    .foldLeft(input.head){(a, b) =>
      (a + b).reduce
    }.magnitude
  def part2(input: ParsedInput) =
    def pairs(s: Seq[Pair]): Seq[(Pair, Pair)] = 
      if s.nonEmpty then
        val head = s.head
        val tail = s.tail
        tail
        .flatMap{p => 
          Seq((head, p),(p, head))
         } ++ pairs(tail)
      else
        Seq()

    pairs(input)
    .map{ (a, b) => 
      (a + b).reduce.magnitude
    }.max