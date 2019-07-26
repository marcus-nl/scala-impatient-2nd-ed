// chapter 11

import scala.math._

// 1
// + and - have the same precedence and are left associative, so:
// 3 + 4 -> 5 == (3 + 4) -> 5
// 3 -> 4 + 5 == (3 -> 4) + 5

// 2
// **: dunno... the precedence would not be appropriate?
// ^: already defined as xor

// 3
class Fraction(n: Int, d: Int) {
  private val num: Int = if (d == 0) 1 else n * signum(d) / gcd(n, d)
  private val den: Int = if (d == 0) 0 else d * signum(d) / gcd(n, d)
  override def toString = s"$num/$den"
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

val a = Fraction(15, -6)
val b = Fraction(3, 4)
a + b
Fraction(1,2) + Fraction(3,4)
Fraction(1,2) - Fraction(3,4)
Fraction(1,2) * Fraction(3,4)
Fraction(1,2) / Fraction(3,4)

// 4
class Amount(w: Int, f: Int) {
  def whole = if (f >= 0) (w + f / 100) else (w - 1 - f / 100)
  def fract = if (f >= 0) (f % 100) else (100 - abs(f) % 100)
  override def toString = s"€ $whole,$fract"
  def == (that: Amount) = this.whole == that.whole && this.fract == that.fract
  def < (that: Amount) = this.whole < that.whole || this.whole == that.whole && this.fract < that.fract
  def + (that: Amount) = Amount(this.whole + that.whole, this.fract + that.fract)
  def - (that: Amount) = Amount(this.whole - that.whole, this.fract - that.fract)
  // * and / operators with Amount arguments would make no sense, but this does:
  def * (mlt: Int) = Amount(this.whole * mlt, this.fract * mlt)
  def / (div: Int) = Amount(this.whole / div, this.fract / div)
}
object Amount {
  def apply(w: Int, f: Int) = new Amount(w, f)
}
Amount(3, 14)
Amount(2, 114)
Amount(1, 10) + Amount(2, 4)
Amount(4, -86)
Amount(4, 42) - Amount(1, 88)
Amount(3, 14) * 42
Amount(33, 33) / 3
Amount(3, 1) < Amount(2, 14)
Amount(3, 14) == Amount(1, 10) + Amount(2, 4)

// 5
class Table(initial: String) {
  val value = initial

  override def toString = "<table>" + value + "</tr></table>"
  def | (s: String) = new Table(value + "<td>" + s + "</td>")
  def || (s: String) = new Table(value + "</tr>\n<tr>" + "<td>" + s + "</td>")
}
object Table {
  def apply() = new Table("<tr>")
}
Table() | "Java"  | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM, .NET"
