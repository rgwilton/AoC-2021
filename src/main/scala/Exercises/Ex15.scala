package aoc

import scala.collection.mutable

object Ex15 extends Exercise:
  type ParsedInput = Array[Array[Int]]

  inline val Max = 10000

  def addBorder(grid: Array[Array[Int]]) =
    val len = grid(0).length
    val maxRow = (for i <- 1 to len + 4 yield Max).toArray
    val newRows = grid.map(line => (mutable.ArrayBuffer(Max, Max) ++ line ++ Seq(Max, Max)).toArray)
    (mutable.ArrayBuffer(maxRow, maxRow) ++ newRows ++ Iterable(maxRow, maxRow)).toArray

  def parseInput(input: Iterator[String]) =
    // All values are decreased by 1 to make part 2 slightly easier.
    (input.map(line => line.map(_ - '1').toArray)).toArray

  case class Candidate(totalCost: Int, x: Int, y: Int) extends Ordered[Candidate]:
    // Order by minimal totalCost and maxinum x+y (i.e., closest to the target).
    def compare(that: Candidate): Int = 
      if totalCost != that.totalCost then 
        that.totalCost - this.totalCost
      else
        (this.x + this.y) - (that.x + that.y)
  
  // Run the shortest path calculation.
  def calc(grid: Array[Array[Int]]): Int =
    // Add a 2 wide border of max values to avoid boundary checks. 
    val risks = addBorder(grid)
    val yLen = risks.length
    val xLen = risks(0).length
    val lowestRisk = Array.fill(xLen, yLen)(Max)
    val xMax = xLen - 3
    val yMax = yLen - 3

    // println("Risks: ")
    // for x <- 0 until xLen do
    //   println(risks(x).map(v => f"$v%-3s").mkString(" "))
    // println("")

    lowestRisk(2)(2) = 0
    val candidates = mutable.PriorityQueue(Candidate(0, 2, 2))

    def calcRisk(xPos: Int, yPos: Int) = 
      if lowestRisk(xPos)(yPos) == Max then
        inline def lr(x: Int, y: Int) = lowestRisk(x)(y)
        val minLr = lr(xPos - 1, yPos) min lr(xPos + 1, yPos) min lr(xPos, yPos - 1) min lr(xPos, yPos + 1)
        val cost = minLr + risks(xPos)(yPos) + 1
        lowestRisk(xPos)(yPos) = cost
        candidates.enqueue(Candidate(cost, xPos, yPos))

    while lowestRisk(xMax)(yMax) == Max do
      val candidate = candidates.dequeue
      val cx = candidate.x
      val cy = candidate.y
      calcRisk(cx + 1, cy)
      calcRisk(cx, cy + 1)
      calcRisk(cx - 1, cy)
      calcRisk(cx, cy - 1)

    lowestRisk(xMax)(yMax)  

  def part1(input: ParsedInput) = calc(input)
  
  def part2(input: ParsedInput) =
    // Expand each row across 5 times.
    val expandedRows = 
      for row <- input yield
        val a = mutable.ArrayBuffer[Int]()
        for repeat <- 0 to 4 do
          a.appendAll(row.map(x => (x + repeat) % 9))
        a.toArray

    // Expand the rows down 5 times.
    val expandedBoard = 
      val a = mutable.ArrayBuffer[Array[Int]]()
      for repeat <- 0 to 4 do
        for row <- expandedRows do
          a.append(row.map(x => (x + repeat) % 9))
      a.toArray

    calc(expandedBoard)