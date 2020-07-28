// chapter 4
import java.io.File
import java.util
import java.util.Scanner

import scala.collection.JavaConverters._

// 1
val prices = Map("bike" -> 500.0, "vacuum" -> 110.95, "mattress" -> 400.0, "tesla" -> 100000.0)
val discounts = prices.mapValues(_ * 0.90)

// 2
val file = new File("inputs/longwords.txt")
def iterateWordsInFile(f: File): Iterator[String] =
  new Scanner(f).asScala

val words = scala.collection.mutable.Map[String, Int]()
for (w <- iterateWordsInFile(file))
  words(w) = words.getOrElse(w, 0) + 1
for ((w, c) <- words) println(s"$w: $c")

// 3
var immuWords = scala.collection.Map[String, Int]()
for (w <- iterateWordsInFile(file))
  immuWords += (w -> (immuWords.getOrElse(w, 0) + 1))
for ((w, c) <- immuWords) println(s"$w: $c")

// 4
val sortedWords = scala.collection.mutable.SortedMap[String,Int]()
for (w <- iterateWordsInFile(file))
  sortedWords += (w -> (sortedWords.getOrElse(w, 0) + 1))
for ((w, c) <- sortedWords) println(s"$w: $c")

// 5
val treeMap = new util.TreeMap[String,Int]().asScala
for (w <- iterateWordsInFile(file)) {
  treeMap += (w -> (treeMap.getOrElse(w, 0) + 1))
}
for ((w, c) <- sortedWords) println(s"$w: $c")

// 6
val weekdays = scala.collection.mutable.LinkedHashMap(
  "Monday" -> java.util.Calendar.MONDAY,
  "Tuesday" -> java.util.Calendar.TUESDAY,
  "Wednesday" -> java.util.Calendar.WEDNESDAY,
  "Thursday" -> java.util.Calendar.THURSDAY,
  "Friday" -> java.util.Calendar.FRIDAY
)
for ((d, c) <- weekdays) println(s"$d -> $c")

// 7
val props = System.getProperties.asScala
val maxKeyLength = props.keySet.map(_.length).max

for ((k,v) <- props) {
  val pk = k.padTo(maxKeyLength, ' ')
  println(s"$pk | $v")
}

// 8
def minmax(a: Array[Int]) =
  (a.min, a.max)
minmax(Array(5,1,3,7,-1,3))

// 9
def lteqgt(a: Array[Int], v: Int) = {
  (a.count(_ < v), a.count(_ == v), a.count(_ > v))
}
lteqgt(Array(5,1,3,7,-1,3), 3)

// 10
"Hello".zip("World")
"Hello".zip("World!")
// plausible use case could be to compare the strings by character:
for ((l, r) <- "Hello".zip("World")) yield l == r


