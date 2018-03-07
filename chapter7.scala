// 1

import com.horstmann.impatient.{Excercise1OnePackage, Excercise1SeperatePackages}

Excercise1OnePackage.hello()
Excercise1SeperatePackages.hello()

// 2: ?

// 3
//import chapter7.random._
//setSeed(1234)
//nextInt()
//nextDouble()

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

// 9: see SecureHello

// 10: Array, Byte, Boolean, Char, Double etc