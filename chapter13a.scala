// chapter 13
import scala.collection.mutable._

// 1
// first attempt: not sorted
def indexes1(s: String) =
  s.zipWithIndex.groupBy(p => p._1).map(p => (p._1, p._2.map(q => q._2)))
indexes1("Mississippi")

// now it's sorted
def indexes(s: String) = {
  val charsToIndices = new HashMap[Char,Set[Int]] with MultiMap[Char,Int] {
    override protected def makeSet: Set[Int] = new TreeSet[Int]
  }
  for (i <- s.indices) {
    charsToIndices.addBinding(s(i), i)
  }
  charsToIndices
}
indexes("Mississippi")
