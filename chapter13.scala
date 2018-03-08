// chapter 13 - continued
import scala.collection.immutable._
import scala.collection.mutable.ListBuffer
import scala.io.Source

// 1
def indexes1(s: String) = {
  import scala.collection.mutable._
  val charsToIndices = new HashMap[Char,Set[Int]] with MultiMap[Char,Int] {
    override protected def makeSet: Set[Int] = new TreeSet[Int]
  }
  for (i <- s.indices) charsToIndices.addBinding(s(i), i)
  charsToIndices
}
indexes1("Mississippi")

// 2
def indexes2a(s: String) =
  s.zipWithIndex.groupBy(p => p._1).map(p => (p._1, p._2.map(q => q._2)))
indexes2a("Mississippi")

// or using fold and immutable maps
def indexes2b(s: String): Map[Char,List[Int]] = {
  val initial = HashMap[Char,List[Int]]().withDefaultValue(Nil)
  s.zipWithIndex.foldLeft(initial)((map, pair) =>
    map + (pair._1 -> (pair._2 :: map(pair._1))))
}
indexes2b("Mississippi")

// other approach
def merge[K,V,X](map: Map[K,V], entry: (K,X))(fun: (V, X) => V) = entry match {
  case (k, v) => map + (k -> fun(map(k), v))
}
def indexes2c(s: String): Map[Char,List[Int]] = {
  val initial = HashMap[Char,List[Int]]().withDefaultValue(Nil)
  s.zipWithIndex.foldLeft(initial)(merge(_, _)((lst, i) => i :: lst))
}
indexes2c("Mississippi")

// other approach for merge
def merge2[K,V](map: Map[K,V], entry: (K,V))(fun: (V, V) => V) = entry match {
  case (k, v) => map + (if (map.contains(k)) (k -> fun(map(k), v))else (k -> v))
}
def indexes2d(s: String): Map[Char,List[Int]] = {
  val initial = Map[Char,List[Int]]()
  s.zipWithIndex.foldLeft(initial)((map, pair) =>
    merge2(map, (pair._1 -> List(pair._2)))(_ ::: _))
}
indexes2d("Mississippi")

// 3
def half1(b: ListBuffer[Int]) = {
  for (i <- b.indices.reverse if (i % 2 == 1)) b.remove(i)
  b
}
def half2(b: ListBuffer[Int]) = {
  var r = new ListBuffer[Int]
  for (i <- b.indices if (i % 2 == 0)) r += b(i)
  r
}

// probably slow because the elements have to be shifted for every deleted character
half1(ListBuffer(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
// therefore this one should be faster, but I didn't measure it
half2(ListBuffer(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

// 4
def corresponding(strs: Array[String], map: Map[String,Int]) =
  strs.flatMap(map.get(_))

val strs = Array("Tom", "Fred", "Harry")
val map = Map("Tom" -> 3, "Dick" -> 4, "Harry" -> 5)
corresponding(strs, map)

// 5
// using only reduceLeft, but only supporting strings:
def mkStringImitator(strs: Seq[String], between: String = ", "): String =
  strs.reduceLeft((a, b) => a + between + b)

mkStringImitator(List("Foo", "Bar", "Quux"), " | ")

// generic but using map in addition to reduceLeft:
def mkStringImitator2[T](strs: Seq[T], between: String = ", "): String =
  strs.map(_.toString).reduceLeft((a, b) => a + between + b)

mkStringImitator2(List("Foo", "Bar", "Quux"), " | ")

// 6
val lst = (1 to 16).toList
// 1st: fold right lst, starting with an empty list,
// combining successive elements by prepending them to the accumulating list,
// so effectively recreating the list
(lst :\ List[Int]())(_ :: _)
// 2nd: same but using fold left and appending (so less efficient I think)
(List[Int]() /: lst)(_ :+ _)
// reverse:
(lst :\ List[Int]())((a, b) => b :+ a)
(List[Int]() /: lst)((a, b) => b :: a)

// 7
val prices = List(5.0, 20.0, 9.95)
val quants = List(10, 2, 1)

(prices zip quants).map(((a: Double, b: Int) => a * b).tupled)
(prices zip quants).map(((_: Double) * (_: Int)).tupled) // a bit shorter

// 8
def grouped(a: Seq[Double], cols: Int) =
  a.grouped(cols).toList

grouped(List(1, 2, 3, 4, 5, 6, 7), 3)

// 9
// why flatMap: otherwise the result would be a nested collection
// what happens if...: two flatMaps

// 10
val ids = java.util.TimeZone.getAvailableIDs.toSeq
def getContinent(id: String) = id.split('/').head
ids.groupBy(getContinent).reduce((a, b) => if (a._2.length > b._2.length) a else b)._1

// 11
def mergeAll[K,V](a: Map[K,V], b: Map[K,V])(fun: (V, V) => V): Map[K,V] = {
  def mergeEntry(entry: (K, V)) = entry match {
    case (k, v) => (k, if (a.contains(k)) fun(a(k), v) else v)
  }
  b.map(mergeEntry)
}

val fn3 = """~/scala/longwords.txt"""
val str = Source.fromFile(fn3).mkString
val initial = Map[Char, Int]()
str.par.aggregate(initial)((map, ch) => merge2(map, (ch -> 1))(_ + _),
  (a, b) => mergeAll(a, b)(_ + _))
