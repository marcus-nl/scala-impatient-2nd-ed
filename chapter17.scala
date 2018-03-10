// chapter 17
import java.time.LocalDate

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.StdIn
import scala.xml.Node
import scala.xml.parsing.XhtmlParser

// helper
def await[T](f: Future[T]) = Await.result(f, 10.seconds)

// 1
def future(n: Int) = Future { Thread.sleep(1000); n }

for (n1 <- future(2); n2 <- future(40))
  println(n1 + n2)

// equivalent to:
future(2).flatMap(n1 => future(40).map(n2 => println(n1 + n2)))

// println is executed by the 2nd future's thread

// 2 - exercise states return type to be "T => Future[U]", but that makes no sense
def doInOrder[T,U,V](f: T => Future[U], g: U => Future[V]): T => Future[V] = {
  x => f(x) flatMap g
}

def f(n: Int) = Future { n + 1 }
def g(n: Int) = Future { n * 2 }
await(doInOrder(f, g)(10)) // 22

// 3
def doAllInOrder[T](fs: (T => Future[T])*): T => Future[T] =
  fs.reduceLeft((a, b) => x => a(x) flatMap b)

def h(n: Int) = Future { n - 1 }
await(doAllInOrder(f, g, h)(10)) // 21

// 4
def doTogether[T,U,V](f: T => Future[U], g: T => Future[V]): T => Future[(U,V)] = {
  x => {
    val ff = f(x); val gf = g(x)
    for (n1 <- ff; n2 <- gf) yield (n1, n2)
  }
}

await(doTogether(f, g)(10)) // (11,20)

// 5
def all[T](futures: Seq[Future[T]]): Future[Seq[T]] = {
  val initial = Future { List[T]() }
  futures.foldRight(initial)((fut, rsf) => rsf.flatMap(rs => fut.map(r => r :: rs)))
}

await(all(List(f(10), g(20), h(30)))) // List(11, 40, 29)

// 6
def repeat[T](action: => T, until: T => Boolean): Future[T] = {
  @tailrec def doRepeat(action: => T, until: T => Boolean): T = {
    val r = action
    if (until(r)) r else doRepeat(action, until)
  }
  val p = Promise[T]
  Future { p.success(doRepeat(action, until)) }
  p.future
}
/*
repeat(StdIn.readLine("Enter secret password: "), (pwd: String) => pwd == "secret" )
*/
// 7
def countPrimes(n: BigInt): Future[Int] = {
  val cores = Runtime.getRuntime.availableProcessors()
  (BigInt(1) to n)
    .grouped((n / cores).toInt)
    .map(g => Future {
      g.count(_.isProbablePrime(100))
    })
    .reduce((af, bf) => {
      for (a <- af; b <- bf) yield a + b
    })
}

await(countPrimes(1000)) // 168
await(countPrimes(10000)) // 1229
//await(countPrimes(100000)) // 9592
//await(countPrimes(1000000)) // 78498

// 8
object Links extends App {

  def futureLinks =
    for (url <- inputUrl(); root <- loadUrl(url); links <- toLinks(root))
      yield links

  Await.result(futureLinks, 1.minute)
    .foreach(println)

  def inputUrl(): Future[String] = Future { StdIn.readLine("Enter URL: ") }

  def loadUrl(url: String): Future[Node] = Future {
    val source = scala.io.Source.fromURL(url, "UTF-8")
    val parser = new XhtmlParser(source)
    parser.initialize.document.docElem
  }

  def toLinks(root: Node): Future[Seq[String]] = Future {
    for (a <- root \\ "a"; href <- a.attribute("href"))
      yield href.text
  }
}

// 9 - skipped

// 10 - skipped

// 11
// using the global execution context, only n futures will be evaluated right away,
// where n is the number of cores. that first "batch" will all report approximately
// the same time. the next batch will be started after the first is done,
// so will report a time 10 seconds later than the previous batch, etc.
// this won't happen with a cached thread pool, since a thread will be created for each future.

// 12 - skipped

// 13
def isPalindrome(s: String) = {
  val middle = s.length / 2
  val start = s.take(middle)
  val end = s.takeRight(middle)
  start == end.reverse
}

isPalindrome("hello")
isPalindrome("noon")
isPalindrome("redivider")

def findPalindromicPrime(range: NumericRange.Inclusive[BigInt]): Future[BigInt] = {
  val p = Promise[BigInt]
  val cores = Runtime.getRuntime.availableProcessors()
  range
    .grouped((range.size / cores))
    .foreach(group => Future {
      group.foreach(n => {
        if (n % 1000 == 0 && p.isCompleted)
          throw new Exception("completed")
        if (n.isProbablePrime(100) && isPalindrome(n.toString))
          p.success(n)
      })
    })
  p.future
}

await(findPalindromicPrime(BigInt(1000) to 100000))
