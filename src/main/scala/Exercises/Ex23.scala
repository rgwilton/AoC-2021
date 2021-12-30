package aoc

import scala.collection.mutable

object Ex23 extends Exercise:
  type ParsedInput = Seq[String]

  trait Position:
    def toString: String

  case object Empty extends Position:
    override def toString = "."

  case class Amphipod(typ: Char) extends Position:
    val home = typ - 'A'
    override def toString = typ.toString
    val stepCost = 
      home match
        case 0 => 1
        case 1 => 10
        case 2 => 100
        case 3 => 1000

  object Room:
    final val homeRooms = 
      (for l <- 0 to 3 yield
        (for c <- 0 to 4 yield HomeRoom(c, l)).toArray).toArray
    def getHomeRoom(count: Int, location: Int) = homeRooms(location)(count)
    def getEmptyRoom(location: Int) = homeRooms(location)(0)

  trait Room:
    def location: Int
    def count: Int

  case class HomeRoom(count: Int, location: Int) extends Room:
    val xCoord = (location + 1) * 2
    def addOne: HomeRoom = Room.getHomeRoom(count + 1, location)

  case class MixedRoom(positions: List[Amphipod], location: Int) extends Room:
    val xCoord = (location + 1) * 2
    val count = positions.length
    def nextAmphipod =
      val a = positions.head
      val r = 
        if positions.tail.forall(_.home == location) then 
          Room.getHomeRoom(positions.length - 1, location)
        else
          this.copy(positions = positions.tail)
      (a, r)

// #############
// #..X.X.X.X..#  Corridor is numbered from 0.
// ###.#.#.#.###  Room 1 to 4, y position is 1 for top, 2 for bottom.
//   #.#.#.#.#
//   #########
  case class Game(corridor: IndexedSeq[Position],
                  rooms: IndexedSeq[Room],
                  roomSize: Int):
    // def print =
    //   println("#############")
    //   println(corridor.mkString("#","","#"))
    //   println(rooms.map(_.head).mkString("###","#","###"))
    //   println(rooms.map(_.p2).mkString("  #","#","#  "))
    //   println("  #########  ")
    //   println()

    def corridorValidPositions = Seq(0, 1, 3, 5, 7, 9, 10)
    def corridorClear(start: Int, end: Int) =
      val positions = 
        if start < end then
          (start + 1) to end
        else
          end to (start - 1)
      positions.forall(p => corridor(p) == Empty)

    def isComplete = rooms.forall(r => r.isInstanceOf[HomeRoom] && r.count == roomSize)

    def allMoves(curCost: Int) = 
      corridor.zipWithIndex.flatMap { (position, index) =>
        position match
          case Empty => None
          case amp:Amphipod =>
            val targetRoom = rooms(amp.home)
            targetRoom match
              case hr@HomeRoom(count, location)
                if corridorClear(index, hr.xCoord) =>
                val steps = (hr.xCoord - index).abs + roomSize - count
                Some(this.copy(
                       corridor = corridor.updated(index, Empty),
                       rooms = rooms.updated(amp.home, hr.addOne)),
                     curCost + (steps * amp.stepCost))
              case _ => None
      } ++
      rooms.collect{ case mr: MixedRoom => mr }.flatMap { mr => 
        corridorValidPositions
        .filter(pos => corridorClear(mr.xCoord, pos))
        .map { pos =>
          val steps = (mr.xCoord - pos).abs + 1 + roomSize - mr.count
          val (amp, updatedRoom) = mr.nextAmphipod
          (this.copy(
            corridor = corridor.updated(pos, amp),
            rooms = rooms.updated(mr.location, updatedRoom)),
            curCost + (steps * amp.stepCost))
        }
      }

// Example game, corridor along the top, room along the bottom.
//  0  --->   10
// #############
// #..X.X.X.X..#  Corridor is numbered from 0.
// ###.#.#.#.###  Room 1 to 4, y position is 1 for top, 2 for bottom.
//   #.#.#.#.#
//   #########

  val StartRegex = """..#(\w)#(\w)#(\w)#(\w)#.*""".r
  def input2game(input: Iterable[String]): Game =
    val startPos = 
      for StartRegex(p1, p2, p3, p4) <- input.toSeq yield 
        Seq(p1, p2, p3, p4).map(str => Amphipod(str(0)))
    val roomSize = startPos.length
    val rooms = 
      for location <- 0 to 3 yield
        val positions = for p <- 0 until roomSize yield 
          startPos(p)(location)
        MixedRoom(positions.toList, location)
    Game(corridor = 0 to 10 map(_ => Empty), rooms.toIndexedSeq, roomSize)

  def parseInput(input: Iterator[String]) = input.toSeq

  def solve(startGame: Game): Int =
    var count = 0
    // Cache positions that we have already seen, and only consider new games
    // if they reach the same state at lower cost.
    // Also compare against the best cost.
    // Use a priority queue of next moves and choose the most expensive each time.
    val incompleteGames = mutable.Map[Game, Int]()
    var bestCompletedCost = Integer.MAX_VALUE
    object QOrdering extends Ordering[(Game, Int)] {
      def compare(a:(Game, Int), b:(Game, Int)) = b._2 compare a._2
    }
    var nextMoveQ = mutable.PriorityQueue[(Game, Int)]()(QOrdering)
    nextMoveQ += ((startGame, 0))

    while nextMoveQ.nonEmpty do
      val (cur, cost) = nextMoveQ.dequeue
      count += 1
      if cost < bestCompletedCost then
        if cur.isComplete then
          bestCompletedCost = cost
        else
          incompleteGames.get(cur) match
            case Some(gameCost) if gameCost <= cost =>
            case _ => 
              incompleteGames += cur -> cost
              nextMoveQ ++= cur.allMoves(cost)
    
//    println(s"Count is $count")
//    println(s"Incomplete Solutions size is ${incompleteGames.size}")
    bestCompletedCost

  def part1(input: ParsedInput) =
    val startGame = input2game(input) 
    solve(startGame)

  def part2(input: ParsedInput) =
    // Modify the game, inserting two extra rows:
    val modifiedInput = 
      input.take(3)
      ++ Seq("  #D#C#B#A#", "  #D#B#A#C#") 
      ++ input.drop(3)
    val startGame2 = input2game(modifiedInput)
    solve(startGame2)