package aoc

import scala.collection.mutable

object Ex23 extends Exercise:
  type ParsedInput = Game

  trait Position:
    def toString: String

  case object Empty extends Position:
    override def toString = "."

  case class Amphipod(typ: Char) extends Position:
    def home = typ - 'A'
    override def toString = typ.toString
    val cost = 
      home match
        case 0 => 1
        case 1 => 10
        case 2 => 100
        case 3 => 1000

  object Room:
    def xCoord(room: Int) = (room + 1) * 2

  case class Room(p1: Position, p2: Position):
    def isEmpty = 
      p1 == Empty && p2 == Empty

// #############
// #..X.X.X.X..#  Corridor is numbered from 0.
// ###.#.#.#.###  Room 1 to 4, y position is 1 for top, 2 for bottom.
//   #.#.#.#.#
//   #########
  case class Game(corridor: IndexedSeq[Position],
                  rooms: IndexedSeq[Room]):
    def print =
      println("#############")
      println(corridor.mkString("#","","#"))
      println(rooms.map(_.p1).mkString("###","#","###"))
      println(rooms.map(_.p2).mkString("  #","#","#  "))
      println("  #########  ")
      println()

    def corridorValidPositions = Seq(0, 1, 3, 5, 7, 9, 10)
    def corridorClear(start: Int, end: Int) =
      val positions = 
        if start < end then
          (start + 1) to end
        else
          end to (start - 1)
      positions.forall(p => corridor(p) == Empty)

    def isComplete = 
      rooms.zipWithIndex.forall{ (room, rIndex) =>
        def p1Ok = room.p1 match
          case a:Amphipod if a.home == rIndex => true
          case _ => false
        def p2Ok = room.p2 match
          case a:Amphipod if a.home == rIndex => true
          case _ => false
        p1Ok && p2Ok
      }

    def allMoves(curCost: Int) = 
      corridor.zipWithIndex.flatMap { (position, index) =>
        position match {
          case Empty => Iterable()
          case a@Amphipod(x) =>
            val homeRoom = rooms(a.home)
            rooms(a.home) match {
              case Room(Empty, Amphipod(x2))
                if x2 == x && corridorClear(index, Room.xCoord(a.home)) =>
                val cost = (1 + (Room.xCoord(a.home) -  index).abs) * a.cost
                Iterable(
                  (Game(corridor.updated(index, Empty),
                        rooms.updated(a.home, homeRoom.copy(p1 = a))),
                   curCost + cost))
              case Room(Empty, Empty)
                if corridorClear(index, Room.xCoord(a.home)) =>
                val cost = (2 + (Room.xCoord(a.home) -  index).abs) * a.cost
                Iterable(
                  (Game(corridor.updated(index, Empty),
                        rooms.updated(a.home, homeRoom.copy(p2 = a))),
                  curCost + cost))
              case _ => Iterable()
            }
        }
      } ++
      rooms.zipWithIndex.flatMap { (room, rIndex) =>
        room match {
          case Room(a:Amphipod, b:Amphipod)
            if a.home != rIndex || b.home != rIndex =>
            corridorValidPositions
            .filter(i => corridorClear(Room.xCoord(rIndex), i))
            .map { i =>
              val cost = (1 + (Room.xCoord(rIndex) -  i).abs) * a.cost
              (Game(corridor.updated(i, a),
                   rooms.updated(rIndex, room.copy(p1 = Empty))),
               curCost + cost)
            }
          case Room(Empty, a:Amphipod) if a.home != rIndex =>
            corridorValidPositions
            .filter(i => corridorClear(Room.xCoord(rIndex), i))
            .map { i =>
              val cost = (2 + (Room.xCoord(rIndex) -  i).abs) * a.cost
              (Game(corridor.updated(i, a),
                   rooms.updated(rIndex, room.copy(p2 = Empty))),
               curCost + cost)
            }
          case _ => Iterable()
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
  def parseInput(input: Iterator[String]) = 
    val startPos = 
      for StartRegex(p1, p2, p3, p4) <- input.toSeq yield 
        Seq(Amphipod(p1(0)), Amphipod(p2(0)), Amphipod(p3(0)), Amphipod(p4(0)))
    val rooms = startPos(0).zip(startPos(1)).map {
      (a, b) => Room(a, b)
    }
    Game(corridor = 0 to 10 map(_ => Empty),
         rooms.toIndexedSeq)

  def part1(input: ParsedInput) = 
    //input.print
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
    nextMoveQ += ((input, 0))

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

    // def allValidGames(cur: Game, cost: Int): Unit =
    //   count += 1
    //   if cost < bestCompletedCost then
    //     if cur.isComplete then
    //       bestCompletedCost = cost
    //     else
    //       incompleteGames.get(cur) match
    //         case Some(gameCost) if gameCost <= cost =>
    //         case _ => 
    //           incompleteGames += cur -> cost
    //           nextMoveQ ++= cur.allMoves(cost).map((a,b) => (b,a))
      
              //val nextGames = cur.allMoves(cost).sortBy(_._2)
              //nextGames.map(allValidGames)

      // if count < 10 then
      //   cur.print
      // count += 1

      // if nextGames.isEmpty then
      //   if cur.isComplete then
      //     completedGames.add(cur)
      // else
    
    //allValidGames(input, 0)
    println(s"Count is $count")
    println(s"Incomplete Solutions size is ${incompleteGames.size}")
    bestCompletedCost

  def part2(input: ParsedInput) =
    ""