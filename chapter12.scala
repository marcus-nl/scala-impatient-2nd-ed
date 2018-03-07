// chapter 12
import scala.math._

// 1
def values(fun: Int => Int, low: Int, high: Int) =
  (low to high).map(v => (v, fun(v)))

values(x => x * x, -5, 5)

// 2
def maxUsingReduce(a: Array[Int]) =
  if (a.isEmpty) None
  else Some(a.reduceLeft(max(_, _)))

maxUsingReduce(Array(5,1,3))
maxUsingReduce(Array())

// 3: fact(5) = 5 * 4 * 3 * 2 * 1 = 120
def factUsingReduce(n: Int) = signum(n) match {
  case 0 => 1
  case 1 => (1 to n).reduceLeft(_ * _)
  case -1 => -(1 to -n).reduceLeft(_ * _)
}

factUsingReduce(1)
factUsingReduce(5)
factUsingReduce(0)
factUsingReduce(-5)

// 4
def factUsingFold(n: Int) =
  (1 to abs(n)).foldLeft(1)(_ * _) * signum(n)

factUsingFold(1)
factUsingFold(5)
factUsingFold(0)
factUsingFold(-5)

// 5
def largest(fun: Int => Int, inputs: Seq[Int]) =
  inputs.map(fun).max

largest(x => 10 * x - x * x, 1 to 10)

// 6
def largestAt(fun: Int => Int, inputs: Seq[Int]) =
  inputs.
    map(v => (v, fun(v))).
    reduceLeft((p1, p2) => if (p1._2 > p2._2) p1 else p2)._1

largestAt(x => 10 * x - x * x, 1 to 10)

// 7
def adjustToPair(fun: (Int,Int) => Int) =
  (pair: (Int, Int)) => fun(pair._1, pair._2)

adjustToPair(_ * _)((6, 7))
val pairs = (1 to 20) zip (11 to 20)
pairs.map(adjustToPair(_ + _))

// 8
val strs = Array("Hi", "Bar", "Quux", "Hello", "World!")
val lens = Array(2, 3, 4, 5, 6)
strs.corresponds(lens)((str, len) => str.length == len)

// 9
def corres[A,B](a: Seq[A], b: Seq[B], fun: (A, B) => Boolean) =
  (a zip b).forall(p => fun(p._1, p._2))
  //was: (a zip b).map(p => fun(p._1, p._2)).reduce(_ && _)

corres[String,Int](strs, lens, (str, len) => str.length == len)
// "problem": need to explicitly specify the type parameters

// 10
// need call-by-name for first param: no
// need currying: yes

def unless(cond: Boolean)(block: => Unit): Unit = {
  if (!cond) block
}

unless (1 == 2) {
  println("Ok gelukkig")
}
