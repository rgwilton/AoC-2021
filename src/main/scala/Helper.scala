package aoc

// Split a string into an iterator of integers, one per line.
extension (str: Iterator[String])
  def asIntegers: Iterator[Int] = str.map(_.toInt)

extension (str: Array[String])
  def asIntegers: Array[Int] = str.map(_.toInt)

extension (str: String)
  def asIntegers: Array[Int] = str.split(",").asInstanceOf[Array[String]].asIntegers



def measure[R](fn: => R): (R, Double) =
  val t1 = System.nanoTime
  val res = fn
  val t2 = System.nanoTime
  (res, (t2-t1).toDouble/1000000)

