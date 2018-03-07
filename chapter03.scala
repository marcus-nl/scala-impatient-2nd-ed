// chapter 3
import java.awt.datatransfer._
import java.util.TimeZone

import scala.collection.JavaConverters
import scala.collection.mutable._
import scala.util.Random

// 1 (procedural)
def rndarr(n: Int) = {
  val arr = new Array[Int](n)
  for (i <- 0 until n)
    arr(i) = Random.nextInt(n)
  arr
}
rndarr(15)

// 1 (functional)
def rndarrf(n: Int) =
  (for { i <- 0 until n } yield Random.nextInt(n))
rndarrf(10).toArray

// 2
def swapels[T](a: Array[T]): Array[T] = {
  val oddIndices = a.indices.filter(_ % 2 == 1)
  for (i <- oddIndices) {
    val tmp = a(i)
    a(i) = a(i - 1)
    a(i - 1) = tmp
  }
  a
}
swapels(Array(1,2,3,4,5,6))
swapels(Array())
swapels(Array(1))
swapels(Array(1,2,3,4,5,6,7))

// 3
def swapels2[T](a: Array[T]) = {
  for (i <- a.indices) yield {
    if (i % 2 == 1)
      a(i - 1)
    else if (i + 1 < a.length)
      a(i + 1)
    else
      a(i)
  }
}
swapels2(Array(1,2,3,4,5,6)).toArray
swapels2(Array()).toArray
swapels2(Array(1)).toArray
swapels2(Array(1,2,3,4,5,6,7)).toArray

// 4
def posneg(a: Array[Int]) =
  a.filter(_ > 0) ++ a.filter(_ <= 0)
posneg(Array(-1, 5, 0, 3, -5, -2, 10))

// 5
def avg(a: Array[Double]): Double =
  a.sum / a.length
avg(Array(10.0, 5.0, -3.0, 0.0))

// 6
val arr = Array(3.0, 10.0, 5.0, -3.0, 0.0)
arr.sortWith(_ > _)
val bff = ArrayBuffer(3.0, 10.0, 5.0, -3.0, 0.0)
bff.sortWith(_ > _)

// 7
val dups = rndarr(20)
dups.distinct

// 8
def fun8(a: ArrayBuffer[Int]): Unit = {
  println(s"Before: $a")
  a.indices.filter(a(_) < 0).reverse.dropRight(1).foreach(a.remove(_))
  println(s"After: $a")
}
fun8(ArrayBuffer(1,2,3))
fun8(ArrayBuffer(1,5,-3,2,-5,0,6))
fun8(ArrayBuffer(1,2,3,-4,5,-6,7,-8))

// 9: skipped

// 10
def timezones(prefix: String) =
  TimeZone.getAvailableIDs.toArray.
    filter(_.startsWith(prefix)).
    map(_.drop(prefix.length)).
    sorted
timezones("America/")

// 11
val flavors = SystemFlavorMap.getDefaultFlavorMap.asInstanceOf[SystemFlavorMap]
val natives = flavors.getNativesForFlavor(DataFlavor.imageFlavor)
JavaConverters.asScalaBuffer(natives)
