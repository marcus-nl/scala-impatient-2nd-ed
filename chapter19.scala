// chapter 19
import scala.collection.mutable.ArrayBuffer

// 1
class Bug {
  var direction: Int = 1
  var position: Int = 0

  def move(distance: Int): Bug = {
    position += distance * direction
    this
  }
  def turn(): Bug = {
    direction = -direction
    this
  }
  def show(): Bug = {
    //println(position)
    this
  }
}

new Bug().move(4).show().move(6).show().turn().move(5).show()

// 2
object show {}
object thén {} // instead of then to prevent warning
object around {}
object move {}

class FluentBug() {
  var direction: Int = 1
  var position: Int = 0

  def move(distance: Int): FluentBug = { position += distance * direction; this }

  def turn(): FluentBug = { direction = -direction; this }
  def turn(obj: around.type): FluentBug = { direction = -direction; this }

  def doShow(): FluentBug = { /*println(position);*/ this }

  def and(obj: thén.type): FluentBug = this
  def and(obj: show.type): FluentBug = { doShow(); this }
}

val bugsy = new FluentBug
bugsy move 4 and show and thén move 6 and show turn around move 5 and show

// 3
object Title
object Author

class Document(var title: String, var author: String = "Unknown") {
  private var attr: Any = null

  def set(obj: Title.type): Document = { attr = obj; this }
  def set(obj: Author.type): Document = { attr = obj; this }

  def to(arg: String) = {
    attr match {
      case null => throw new Exception("Nothing to set!")
      case Title => title = arg
      case Author => author = arg
    }
    this
  }
}

val book = new Document("Scala for Dummies")
book set Title to "Scala for the Impatient" set Author to "Cay Horstmann"

// 4
class Network {
  class Member(val name: String) {
    val contacts = new ArrayBuffer[Member]()

    override def equals(obj: scala.Any) = obj match {
      case that: Member => this.name == that.name
      case _ => false
    }
  }

  private val members = new ArrayBuffer[Member]()

  def join(name: String) = {
    val m = new Member(name)
    members += m
    m
  }
}

val facer = new Network
val twatfeed = new Network
val m1 = facer.join("John")
val m2 = twatfeed.join("John")
m1 == m2
m1 == m1

// 5 - difference between process1 and process2
type NetworkMember = n.Member forSome { val n: Network }
def process1[M <: NetworkMember](m1: M, m2: M) = (m1, m2)
def process2(m1: NetworkMember, m2: NetworkMember) = (m1, m2)

// 6
def findAbout(arr: Array[Int], n: Int): Either[Int,Int] = {
  var closestIndex = -1
  var smallestDiff = Integer.MAX_VALUE

  for (i <- arr.indices) {
    val v = arr(i)
    if (v == n) return Left(i)

    val diff = Math.abs(v - n)
    if (diff < smallestDiff) {
      closestIndex = i
      smallestDiff = diff
    }

    if (v > n) return Right(closestIndex)
  }
  Right(closestIndex)
}

val arr = Array(1, 2, 4, 8, 16, 32, 64, 128)
findAbout(arr, 4)
findAbout(arr, 6)
findAbout(arr, 16)
findAbout(arr, 25)

// 7
type Closable = { def close(): Unit }
def m(closable: Closable, f: Closable => Unit): Unit = {
  try {
    f(closable)
  }
  finally {
    closable.close()
  }
}

// 8
def printValues(f: {def apply(i: Int): Int}, from: Int, to: Int): Unit = {
  (from to to).foreach(i => println(f(i)))
}

val f: Function[Int,Int] = x => x * x
//printValues(f, 3, 6) // hmm... causes NoSuchMethodException: lambda.apply(int)
printValues(Array(1, 1, 2, 3, 5, 8, 13, 21, 23, 55), 3, 6)

// 9
abstract class Dim[T](val value: Double, val name: String) {
  self =>
  protected def create(v: Double): T //self.type
  def +(other: Dim[T]) = create(value + other.value)
  override def toString = s"$value $name"
}

class Seconds(v: Double) extends Dim[Seconds](v, "s") {
  override def create(v: Double): Seconds = new Seconds(v)
}

class Meters(v: Double) extends Dim[Seconds](v, "s") {
  override def create(v: Double) = ??? //new Seconds(v)
}
