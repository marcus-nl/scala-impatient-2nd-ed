// chapter 18

// 1
class Pair[T,S](val first: T, val second: S) {
  def swap: Pair[S,T] = new Pair(second, first)
  override def toString = s"Pair[$first,$second]"
}

new Pair(1, 2).swap

// 2
class MutPair[T](var first: T, var second: T) {
  def swap() = {
    val tmp = first
    first = second
    second = tmp
    this
  }
  //def replaceFirst[R <: T](newFirst: R) { first = newFirst }
  override def toString = s"MutPair[$first,$second]"
}

new MutPair(1, 2).swap()

// 3
def swap[T,S](pair: Pair[T,S]): Pair[S,T] =
  new Pair(pair.second, pair.first)

// 4
class Person(val name: String) {}
class Student(name: String) extends Person(name) {}

class TPair[T](val first: T, val second: T) {
  def replaceFirst(newFirst: T) = new TPair(newFirst, second)
}

val tp = new TPair(new Person("John"), new Person("Mary"))
val tq = tp.replaceFirst(new Student("John"))
// why don't we need..? because the pair's components are still Persons

// 5
// that way the value we're comparing with doesn't need to be wrapped in a RichInt

// 6
def middle[T](it: Iterable[T]): T = {
  it.drop((it.size / 2)).head
}

middle("1234")
middle("World")
middle(List(1))

// 7 - there are a lot of methods...

// 8
// because then it would no longer be a MutPair[T]

// 9 - example that doesn't compile
/*
class BasePair[+T](val first: T, val second: T) {
  // Error: covariant type T occurs in invariant position
  def replaceFirst(newFirst: T) = new BasePair(newFirst, second)
  override def toString = s"Pair[$first,$second]"
}

class NastyDoublePair(first: Double, second: Double) extends BasePair[Double](first, second) {
  override def replaceFirst(newFirst: Double) = new NastyDoublePair(newFirst, newFirst * newFirst)
}

val nastyDoublePair: BasePair[Any] = new NastyDoublePair(1.0, 2.0)
nastyDoublePair.replaceFirst("Hello")
*/
// 10
class MPair2[T,S](var first: T, var second: S) {
  def swap()(implicit ev: T =:= S) = {
    val tmp = first
    first = second.asInstanceOf[T]
    second = tmp
    this
  }
  override def toString = s"MPair2[$first,$second]"
}

new MPair2(1, 2).swap()
new MPair2("A", "B").swap()
//new MPair2(1, "One").swap() // Error: cannot prove that Int =:= String
