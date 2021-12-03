package aoc

import util.chaining.scalaUtilChainingOps

// Split a string into an iterator of integers, one per line.
extension (str: Iterator[String])
  def asIntegers: Iterator[Int] = str.map(_.toInt)

extension (str: Array[String])
  def asIntegers: Array[Int] = str.map(_.toInt)

extension (str: String)
  def asIntegers: Array[Int] = str.split(",").asInstanceOf[Array[String]].asIntegers

extension (x: Int)
  def hasBitSet(index: Int): Boolean =
    inline def mask = 1 << index
    (x & mask) != 0

def sumArrayInPlace(a: Array[Int], b: Array[Int]) = 
  for i <- 0 until a.length do a(i) += b(i)
  a

def sumArray(a: Array[Int], b: Array[Int]) = 
  (for i <- 0 until a.length yield a(i) + b(i)).toArray

// Add a pipe operator
extension [A,B](a: A)
  def  |> (f: (A) => B): B = a.pipe(f)



def measure[R](fn: => R): (R, Double) =
  val t1 = System.nanoTime
  val res = fn
  val t2 = System.nanoTime
  (res, (t2-t1).toDouble/1000000)

