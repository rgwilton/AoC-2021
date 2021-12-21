package aoc

import scala.collection.mutable

object Ex21 extends Exercise:
  type ParsedInput = Seq[StartingPlayer]
  case class StartingPlayer(id: Int, startingPosition: Int)

  val InputRegex = """Player (\d+) starting position: (\d+)""".r
  def parseInput(input: Iterator[String]) = 
    for InputRegex(player, start) <- input.toSeq yield StartingPlayer(player.toInt, start.toInt)

  class Player(id: Int, start: Int):
    var position = start - 1
    var score = 0
    def won = score >= 1000
    def addDiceThrow(diceSum: Int) = 
      position = (position + diceSum) % 10
      score += 1 + position

  def part1(input: Seq[StartingPlayer]) =
    val players@Seq(p1, p2) = input.map(p => Player(p.id, p.startingPosition))
    val dice = Iterator.from(1).flatMap(_ => 1 to 100)
    var diceThrowCount = 0
    val playerTurns = Iterator.from(1).flatMap(_ => players)
    def noWinner(p: Player) = !p1.won && !p2.won
    for (player <- playerTurns.takeWhile(noWinner)) do
      val diceSum = dice.take(3).sum
      diceThrowCount += 3
      player.addDiceThrow(diceSum)
    // Loop finished, one player has won.
    (p1.score min p2.score) * diceThrowCount
    
  def part2(input: Seq[StartingPlayer]) =
    // 3 dice map to their frequency of occurence.
    val diceMap = 
      (for d1 <- 1 to 3;
           d2 <- 1 to 3;
           d3 <- 1 to 3 yield d1 + d2 + d3).groupBy(identity).mapValues(_.length.toLong).toArray

    case class Game(p1Pos: Int, p2Pos: Int, p1Score: Int, p2Score: Int)

    var player1WinCount = 0L
    var player2WinCount = 0L

    // Map from all possible running game states to the number of occurrences.
    val initialRecord = Game(input(0).startingPosition - 1, input(1).startingPosition - 1, 0, 0)
    var scores = mutable.Map[Game, Long](initialRecord -> 1)

    while scores.nonEmpty do
      // Player 1 turn
      // Use a new map so that all turns are processed in parallel.
      val newScores = mutable.Map[Game, Long]()
      for (game, score) <- scores do
        for (sum, count) <- diceMap.iterator do
          val p1NewPos = (game.p1Pos + sum) % 10
          val p1NewScore = game.p1Score + p1NewPos + 1
          if p1NewScore <= 20 then
            val newRecord = Game(p1NewPos, game.p2Pos, p1NewScore, game.p2Score)
            newScores.updateWith(newRecord) {
              case Some(v) => Some(v + score * count)
              case None => Some(score * count)
            }
          else 
            player1WinCount += count * score

      // Player 2 turn
      scores = newScores
      val newScores2 = mutable.Map[Game, Long]()
      for (game, score) <- scores do
        for (sum, count) <- diceMap.iterator do
          val p2NewPos = (game.p2Pos + sum) % 10
          val p2NewScore = game.p2Score + p2NewPos + 1
          if p2NewScore <= 20 then
            val newRecord = Game(game.p1Pos, p2NewPos, game.p1Score, p2NewScore)
            newScores2.updateWith(newRecord) {
              case Some(v) => Some(v + score * count)
              case None => Some(score * count)
            }
          else 
            player2WinCount += count * score
      scores = newScores2

    player1WinCount max player2WinCount