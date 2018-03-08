// chapter 15

// 1 - skipped

// 2
@deprecated("Class")
class VeryAnnotated @deprecated("Primary Constructor") (@deprecated("Primary Constructor Param") initial: Int) {

  @deprecated("Field")
  val field = initial

  @deprecated("Secondary Constructor")
  def this() = this(1)

  @deprecated("Method")
  def method(@deprecated("Parameter") param: Int): Unit = {
    @deprecated("Local") val local = 2
    print(s"field=$field, local=$local, param=$param")
  }

  def rest[@deprecated("Type Parameter") T](s: String @deprecated("Type")): Unit = {
    println(s + 1 : @deprecated("Expression"))
  }
}

// 3
// @deprecated uses @getter @setter @beanGetter @beanSetter
// @deprecatedName uses @param
// @BeanProperty, @transient and @volatile use @field

// 4
import scala.annotation.{tailrec, varargs}
@varargs def sum(args: Int*) = args.sum

// 5
object GetLines {
  import scala.io.Source
  import java.io.File

  def from(f: File) =
    Source.fromFile(f).mkString
}
/* Java calling class:
public class GetLinesUser {
	public static void main(String... args) {
		String lines = GetLines.from(new java.io.File(args[0]));
		System.out.println(lines);
	}
}
*/

// 6
object Sleeper extends App {
  @volatile var rested = false

  new Thread() {
    override def run() = {
      Thread.sleep(5000L);
      rested = true;
      println("Okay then")
    }
  }.start()

  new Thread() {
    override def run(): Unit = {
      while (!rested) {
        println("Not ready...")
        Thread.sleep(1000L)
      }
      println("Ready!")
    }
  }.start()
}
// it actually works fine so far without @volatile...

// 7 - obviously it's possible for the overriding method to not be tail recursive any more:
class Rec {
  /*@tailrec*/ def fun(n: Int): Int = if (n == 0) 1 else fun(n - 1)
}
class NotRec extends Rec {
  override def fun(n: Int): Int = if (n == 0) 1 else 1 + fun(n - 1)
}

// 8
object Comp {
  @specialized def allDifferent[T](x: T, y: T, z: T) = x != y && x != z && y != z
}
// variants were generated for: boolean, byte, char, double, float, int, long, short
// and "scala.runtime.BoxedUnit"

// 9 - ???
// public final <U> void foreach(scala.Function1<java.lang.Object, U>);
// public final void foreach$mVc$sp(scala.Function1<java.lang.Object, scala.runtime.BoxedUnit>);

// 10
def fact(n: Int): Int = {
  assert(n >= 0)
  if (n == 0) 1 else n * fact(n - 1)
}
fact(5)
fact(-1) // yields AssertionError when assertions enabled, otherwise StackOverflowError
