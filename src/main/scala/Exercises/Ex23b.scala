package aoc

import scala.collection.mutable

object Ex23b extends Exercise:
  override def input = scala.io.Source.fromFile(s"input/input_23.txt").getLines

  type ParsedInput = Seq[String]

  trait Position:
    def toString: String

  case object Empty extends Position:
    override def toString = "."

  object Amphipod:
    def stepCost(homeLocation: Int) = Math.pow(10, homeLocation).toInt
    val AmphipodsMap = (for l <- 'A' to 'D' yield (l -> Amphipod(l))).toMap
      
  case class Amphipod(typ: Char) extends Position:
    val homeLocation = typ - 'A'
    val homeXCoord = Room.xCoord(homeLocation)

    // These don't include the cost of entering or leaving a room.
    def stepsToHomeRoom(curXCoord: Int) =
      (Room.xCoord(homeLocation) - curXCoord).abs
    def costToHomeRoom(curXCoord: Int) = stepsToHomeRoom(curXCoord) * stepCost
    override def toString = typ.toString

    val stepCost = Amphipod.stepCost(homeLocation) 

  case class GameContext(roomSize: Int)

  object Room:
    def xCoord(location: Int) = (location + 1) * 2
    final val homeRooms = 
      (for l <- 0 to 3 yield
        (for c <- 0 to 4 yield Room(l, List(), c)).toArray).toArray
    def getHomeRoom(count: Int, location: Int) = homeRooms(location)(count)

  case class Room(location: Int, mustLeave: List[Amphipod], inFinalPlace: Int):
    val next = 
      if mustLeave.nonEmpty then this.copy(mustLeave = mustLeave.tail)
      else if inFinalPlace < 4 then this.copy(inFinalPlace = inFinalPlace + 1)
      else this

    val xCoord = Room.xCoord(location)
    def addOne: Room = next
    val hasLeavers = mustLeave.nonEmpty
    val canFill = mustLeave.isEmpty
    def nextAmphipod = (mustLeave.head, next)

    // Any Amphipod not in the final place must leave the room.
    def leaveCost(using gCtx:GameContext) =
      mustLeave.zipWithIndex.map { (amp, i) => (i + 1) * amp.stepCost }.sum

    // Any Amphipod not in the final place must enter the room.
    def fillCost(using gCtx:GameContext) =
      (for i <- 1 to (gCtx.roomSize - inFinalPlace) yield i).sum * Amphipod.stepCost(location)

  object Game:
    def steps(locA: Int, locB: Int) = (locA - locB).abs

  // minRemainingCost is the very minimum cost possible to complete the puzzle.
  case class Game(corridor: IndexedSeq[Position],
                  rooms: IndexedSeq[Room],
                  minRemainingCost: Int):
    // def print =
    //   println("#############")
    //   println(corridor.mkString("#","","#"))
    //   println(rooms.map(_.head).mkString("###","#","###"))
    //   println(rooms.map(_.p2).mkString("  #","#","#  "))
    //   println("  #########  ")
    //   println()

    // The cost to entry/fill the rooms is entirely static so precomute that once and then it can be ignored.
    def calcFixedFillLeaveCost(using gCtx: GameContext) =
      (for r <- rooms yield r.leaveCost + r.fillCost).sum

    def minStepCosts(using gCtx: GameContext) =
      val ampMoveCosts = 
        for r <- rooms
            a <- r.mustLeave yield
          if a.homeLocation == r.location then 
            // Must minimally leave to the corridor and return
            2 * a.stepCost
          else
            a.costToHomeRoom(r.xCoord)
      ampMoveCosts.sum

    def corridorValidPositions = Seq(0, 1, 3, 5, 7, 9, 10)
    def corridorIsClear(start: Int, end: Int) =
      val positions = 
        if start < end then (start + 1) to end else end to (start - 1)
      positions.forall(p => corridor(p) == Empty)

    def isComplete(using gCtx: GameContext) = rooms.forall(_.inFinalPlace == gCtx.roomSize)

    def allMoves(curCost: Int)(using gCtx:GameContext) = 
      corridor.zipWithIndex.flatMap { (maybeAmp, xCoord) =>
        maybeAmp match
          case amp:Amphipod
            if corridorIsClear(xCoord, amp.homeXCoord) && rooms(amp.homeLocation).canFill =>
            val homeRoom = rooms(amp.homeLocation)
            val moveCost = amp.costToHomeRoom(xCoord)
            Some(Game(corridor.updated(xCoord, Empty),
                      rooms.updated(homeRoom.location, homeRoom.addOne),
                      minRemainingCost - moveCost),
                  curCost + moveCost)
          case _ => None
      } ++
      rooms.filter(_.hasLeavers).flatMap { room =>
        val (amp, updatedSrcRoom) = room.nextAmphipod
        val homeRoom = rooms(amp.homeLocation)

        if homeRoom.canFill && corridorIsClear(room.xCoord, homeRoom.xCoord) then
          // Move directly to the home room if possible.
          val moveCost = amp.costToHomeRoom(room.xCoord)
          Seq((Game(corridor,
                    rooms.updated(homeRoom.location, homeRoom.addOne).updated(room.location, updatedSrcRoom),
                    minRemainingCost - moveCost),
                  curCost + moveCost))
        else
          corridorValidPositions
          .filter(xCoord => corridorIsClear(room.xCoord, xCoord))
          .map { xCoord =>
            val (amp, updatedRoom) = room.nextAmphipod
            val moveCost = amp.stepCost * Game.steps(xCoord, room.xCoord)
            val minRemCost =
              if amp.homeLocation == room.location then
                minRemainingCost + amp.costToHomeRoom(xCoord) - (2 * amp.stepCost)
              else
                minRemainingCost - amp.costToHomeRoom(room.xCoord) + amp.costToHomeRoom(xCoord)
            (Game(corridor.updated(xCoord, amp),
                  rooms.updated(room.location, updatedRoom),
                  minRemCost),
              curCost + moveCost)
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
        Seq(p1, p2, p3, p4).map(str => Amphipod.AmphipodsMap(str(0)))
    val roomSize = startPos.length
    val rooms = 
      for location <- 0 to 3 yield
        val allAmps = for p <- 0 until roomSize yield 
          startPos(p)(location)
        val mustLeaveAmps = allAmps.reverse.dropWhile(_.homeLocation == location).reverse
        Room(location, mustLeaveAmps.toList, roomSize - mustLeaveAmps.length)
    Game(corridor = 0 to 10 map(_ => Empty), rooms.toIndexedSeq, 0)

  def parseInput(input: Iterator[String]) = input.toSeq

  def solve(startGame: Game)(using gCtx: GameContext): Int =
    // Cache positions that we have already seen, and only consider new games
    // Give up for any solutions that are already worse than the best soln so far.
    // Use a priority queue of next moves and choose the most expensive each time.
    val incompleteGames = mutable.Map[Game, Int]()
    var bestCompletedCost = Integer.MAX_VALUE

    object QOrdering extends Ordering[(Game, Int)] {
      def compare(a:(Game, Int), b:(Game, Int)) = 
        (b._2 + b._1.minRemainingCost) compare (a._2 + a._1.minRemainingCost)
    }

    var nextMoveQ = mutable.PriorityQueue[(Game, Int)]()(QOrdering)
    nextMoveQ += ((startGame, 0))

    while nextMoveQ.nonEmpty do
      val (cur, cost) = nextMoveQ.dequeue
      if cost + cur.minRemainingCost < bestCompletedCost then
        if cur.isComplete then
          bestCompletedCost = cost
        else
          incompleteGames.get(cur) match
            case Some(gameCost) if gameCost <= cost =>
            case _ => 
              incompleteGames += cur -> cost
              nextMoveQ ++= cur.allMoves(cost)
              
    bestCompletedCost

  def part1(input: ParsedInput) =
    given GameContext = GameContext(roomSize = 2)
    val startGame = input2game(input)
    val fixedCosts = startGame.calcFixedFillLeaveCost
    val soln = solve(startGame.copy(minRemainingCost = startGame.minStepCosts))
    fixedCosts + soln
  def part2(input: ParsedInput) =
    given GameContext = GameContext(roomSize = 4)

    // Modify the game, inserting two extra rows:
    val modifiedInput = 
      input.take(3)
      ++ Seq("  #D#C#B#A#", "  #D#B#A#C#") 
      ++ input.drop(3)
    val startGame = input2game(modifiedInput)
    val fixedCosts = startGame.calcFixedFillLeaveCost
    fixedCosts + solve(startGame.copy(minRemainingCost = startGame.minStepCosts))