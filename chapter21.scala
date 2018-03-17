// chapter 21

// 1
// -> is an implicit method of any type that converts its operands to a pair

// 2
implicit final class PercentageOps(private val self: Double) extends AnyVal {
  @inline def +% (percentage: Double): Double =
    self * (1.0 + (percentage / 100.0))
}
120 +% 10
123.4 +% 3.14

// 3
implicit final class FactorialOps(private val self: Int) extends AnyVal {
  @inline def ! : Int =
    if (self == 0) 1
    else self * (self - 1).!
}

5.!

// or:
import scala.language.postfixOps
5!

// 4 - skipped

// 5
import scala.math.{abs, signum}

class Fraction(n: Int, d: Int) {
  private val num: Int = if (d == 0) 1 else n * signum(d) / gcd(n, d)
  private val den: Int = if (d == 0) 0 else d * signum(d) / gcd(n, d)
  override def toString = s"$num/$den"
  def toDouble = num.toDouble / den.toDouble
  def gcd(a: Int, b: Int): Int = if (b == 0) abs(a) else gcd(b, a % b)
  def + (that: Fraction) = {
    Fraction(this.num * that.den + that.num * this.den, this.den * that.den)
  }
  def - (that: Fraction) = {
    Fraction(this.num * that.den - that.num * this.den, this.den * that.den)
  }
  def * (that: Fraction) = {
    Fraction(this.num * that.num, this.den * that.den)
  }
  def / (that: Fraction) = {
    Fraction(this.num * that.den, this.den * that.num)
  }
}
object Fraction {
  def apply(n: Int, d: Int) = new Fraction(n, d)
}
implicit class RichFraction(private val self: Fraction) extends Ordered[Fraction] {
  override def compare(that: Fraction) = self.toDouble.compare(that.toDouble)
}

def smaller[T](a: T, b: T)(implicit order: T => Ordered[T]): T = {
  if (a < b) a else b
}

Fraction(1, 7) < Fraction(2, 9) // true
Fraction(1, 7) < Fraction(1, 9) // false
smaller(Fraction(1, 7), Fraction(2, 9)) // 1/7
smaller(Fraction(1, 7), Fraction(1, 9)) // 1/9

// 6
import java.awt.Point

implicit class LexographicPointOrdering(private val self: Point) extends Ordered[Point] {
  override def compare(that: Point) =
    if (self.x == that.x)
      self.y.compare(that.y)
    else
      self.x.compare(that.x)
}

new Point(3, 4) < new Point(4, 5) // true
new Point(4, 5) < new Point(4, 4) // false

// 7
implicit class DistanceToOriginPointOrdering(private val self: Point)
  extends Ordered[Point] {
  override def compare(that: Point) =
    self.distanceToOriginSquared.compare(that.distanceToOriginSquared)

  def distanceToOriginSquared = self.x * self.x + self.y * self.y
}

new Point(3, 4) < new Point(4, 1) // false
new Point(4, 5) < new Point(5, 6) // true

// how to switch: by either importing LexographicPointOrdering or DistanceToOriginPointOrdering

// 8 - TODO

// 9
// Ordered is meant to be extended by a type which then has a single ordering,
// so it's NOT a type class.
// Ordering represents a strategy for sorting instances of a type.
// By extending the trait you define such a strategy, thereby joining the type class.

// 10
trait NumberLike[T] {
  def plus(x: T, y: T): T
  def divideBy(x: T, n: Int): T
}
object NumberLike {
  implicit object NumberLikeDouble extends NumberLike[Double] {
    override def plus(x: Double, y: Double) = x + y
    override def divideBy(x: Double, n: Int) = x / n
  }
  implicit object NumberLikeInt extends NumberLike[Int] {
    override def plus(x: Int, y: Int) = x + y
    override def divideBy(x: Int, n: Int) = x / n
  }
}
def average[T : NumberLike](elems: T*) = {
  val numberLike = implicitly[NumberLike[T]]
  val total = elems.reduce(numberLike.plus)
  numberLike.divideBy(total, elems.length)
}

average(1, 2, 3, 4, 5, 100)

// 11 :D
implicit object NumberLikeString extends NumberLike[String] {
  override def plus(x: String, y: String) = x + y
  override def divideBy(x: String, n: Int) =
    (for ((c, i) <- x.zipWithIndex if i % n == 0) yield c).mkString
}

average("Hello", "World")

// 12 - TODO

// 13 - TODO
