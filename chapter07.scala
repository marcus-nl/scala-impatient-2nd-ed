// chapter 7

// 1
/* using the following in seperate files:

package com.horstmann.impatient

object Excercise1OnePackage {
  def hello() {
    println("From here we cannot use 'sun.beans.TypeResolver', but have to use the absolute path: " + classOf[com.sun.beans.TypeResolver])
  }
}

package com
package horstmann
package impatient

object Excercise1SeperatePackages {
  def hello() {
    println("From here we can use 'sun.beans.TypeResolver' without 'com.': " + classOf[sun.beans.TypeResolver])
  }
}
 */
import com.horstmann.impatient.{Excercise1OnePackage, Excercise1SeperatePackages}

Excercise1OnePackage.hello()
Excercise1SeperatePackages.hello()

// 2: skipped

// 3
/* using the following in src/main/scala/chapter7/package.scala:

package chapter7

package object random {
  var previous: Int = 0
  val a = 1664525
  val b = 1013904223
  val n = 32

  def setSeed(seed: Int) { this.previous = seed; }
  def nextInt(): Int = {
    previous = (previous * a + b) % (2 ^ n)
    previous
  }
  def nextDouble(): Double = nextInt().toDouble
}
 */
import chapter7.random._
setSeed(1234)
nextInt()
nextDouble()

// 4: this way it's consistent among classes, objects, package objects.

// 5: visible to the "com" pacakge. could be useful.

// 6
import java.util.{HashMap => JavaHashMap}
import scala.collection.mutable.{HashMap => ScalaHashMap}
def copy[A,B](source: JavaHashMap[A,B], target: ScalaHashMap[A,B]) = {
  import scala.collection.JavaConverters._
  for ((k,v) <- source.asScala) {
    target(k) = v
  }
  target
}
val source: JavaHashMap[String,String] = new JavaHashMap
source.put("Foo", "Bar")
source.put("Quux", "Baz")
copy(source, new ScalaHashMap)

// 7: moved the import of JavaConverters to the copy method

// 8: makes all symbols (subpackage in this case) of those packages visible. no.

// 9
object SecureHello extends App {
  import java.lang.System._
  getProperty("user.name")
  val pwd = scala.io.StdIn.readLine("Enter password: ")
  if (pwd != "secret") {
    Console.err.println("Invalid password")
  } else {
    println("Hello")
  }
}

// 10: Array, Byte, Boolean, Char, Double etc
