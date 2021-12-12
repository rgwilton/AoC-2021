package aoc

import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Success
import scala.util.Failure

@main def aoc(testNos: String*): Unit = 
  val AllExercises = IndexedSeq(Ex1, Ex2, Ex3, Ex4, Ex5b, Ex6, Ex7, Ex8, Ex9, Ex10, Ex11, Ex12)
  val loops = 10  // To measure jitted performance.

  val validTestNos = testNos.flatMap(_.toIntOption).filter(_ <= AllExercises.length)

  val exercises = 
    if validTestNos.isEmpty then AllExercises
    else validTestNos.map(x => AllExercises(x - 1))

  def exRuns = 
    Future.sequence {
      exercises.map { ex =>
        Future {
          val runs = for i <- 1 to loops yield ex.run
          runs.last
        }
      }
    }.andThen {
      case Success(results) =>
        for Result(name, p1, p1t, p2, p2t, time) <- results do    
          println(f"""$name%-5s => part 1: "$p1" in $p1t ms; part 2: "$p2" in $p2t ms, total; $time ms""")
      case Failure(e) => throw e
    }

  Await.ready(exRuns, 10 minutes)


case class Result(name: String, part1: Any, p1Time: Double, part2: Any, p2Time: Double, totalTime: Double)

trait Exercise:
  type ParsedInput
  val name = getClass.getSimpleName.nn.init
  val num = name.drop(2)
  def input = scala.io.Source.fromFile(s"input/input_${num}.txt").getLines

  def parseInput(input: Iterator[String]): ParsedInput
  def part1(input: ParsedInput): Any
  def part2(input: ParsedInput): Any

  def run: Result = run(input)
  def run(input: Iterator[String]): Result =
    measure {
      val parsedInput = parseInput(input)
      (measure { part1(parsedInput) },  measure { part2(parsedInput) })
    } match
      case (((pt1, pt1Time), (pt2, pt2Time)), totalTime) => Result(name, pt1, pt1Time, pt2, pt2Time, totalTime)
  
