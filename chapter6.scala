// chapter 6

// 1
object Conversions {
  def inchesToCentimeters(inches: Double) = inches * 2.54
  def gallonsToLiters(gallons: Double) = gallons * 3.78541
  def milesToKilometers(miles: Double) = miles * 1.60934
}
Conversions.inchesToCentimeters(9.3)
Conversions.gallonsToLiters(3.1)
Conversions.milesToKilometers(27.5)

// 2
trait UnitConversions {
  def apply(v: Double): Double
}
object InchesToCentimeters extends UnitConversions {
  def apply(v: Double) = v * 2.54
}
object GallonsToLiters extends UnitConversions {
  def apply(v: Double) = v * 3.78541
}
object MilesToKilometers extends UnitConversions {
  def apply(v: Double) = v * 1.60934
}
InchesToCentimeters(9.3)
GallonsToLiters(3.1)
MilesToKilometers(27.5)
// better solution: https://github.com/hempalex/scala-impatient/blob/master/Chapter06/02.scala

// 3
class AwtOrigin extends java.awt.Point {}
val origin = new AwtOrigin
origin.setLocation(5, 6) // class is mutable
origin

// 4
class Point private (val x:Int, val y:Int) {}
object Point {
  def apply(x: Int, y: Int) = new Point(x, y)
}

// 5
object Reverse extends App {
  println(args.reverse.mkString(" "))
}
Reverse.main(Array("Hello", "World!"))

// 6 + 7
object Suits extends Enumeration {
  type Suits = Value
  val Clubs     = Value("\u2667") // black
  val Diamonds  = Value("\u2662") // red
  val Hearts    = Value("\u2661") // red
  val Spades    = Value("\u2664") // black
  def isRed(value: Value) = value == Diamonds || value == Hearts
}
import Suits._
Suits.values.foreach(suit => {
  println(s"$suit: isRed=${isRed(suit)}")
})


// 8
object Colners extends Enumeration {
  type Colners = Value
  val Black   = Value(0x000000)
  val Red     = Value(0xff0000)
  val Green   = Value(0x00ff00)
  val Blue    = Value(0x0000ff)
  val Yellow  = Value(0xffff00)
  val Magenta = Value(0xff00ff)
  val Cyan    = Value(0x00ffff)
  val White   = Value(0xffffff)
}

Colners.values.foreach(c => println(f"${c.id}%06x: ${c}%s"))
