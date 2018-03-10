// chapter 17
import scala.annotation.tailrec
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

// 1
def future(n: Int) = Future { Thread.sleep(1000); n }

for (n1 <- future(2); n2 <- future(40))
  println(n1 + n2)

// equivalent to:
future(2).flatMap(n1 => future(40).map(n2 => println(n1 + n2)))

// println is executed by the 2nd future's thread

// 2 - excercise states return type to be "T => Future[U]", but that makes no sense
def doInOrder[T,U,V](f: T => Future[U], g: U => Future[V]): T => Future[V] = {
  x => f(x) flatMap g
}

def f(n: Int) = Future { n + 1 }
def g(n: Int) = Future { n * 2 }
doInOrder(f, g)(10).foreach(n => println(n))

// 3
def doAllInOrder[T](fs: (T => Future[T])*): T => Future[T] =
  fs.reduceLeft((a, b) => x => a(x) flatMap b)

def h(n: Int) = Future { n - 1 }
doAllInOrder(f, g, h)(10).foreach(n => println(n))

// 4
def doTogether[T,U,V](f: T => Future[U], g: T => Future[V]): T => Future[(U,V)] = {
  x => for (n1 <- f(x); n2 <- g(x)) yield (n1, n2)
}

doTogether(f, g)(10).foreach(n => println(n))

// 5 - hmmm
def all[T](futures: Seq[Future[T]]): Future[Seq[T]] = {
  val p = Promise[Seq[T]]
  Future {
    val v = futures.map(f => {
      val r = Await.result(f, 10.seconds)
      r
    })
    p.success(v)
  }
  p.future
}

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
import scala.io.StdIn
repeat(StdIn.readLine("Enter secret password: "), (pwd: String) => pwd == "secret" )
*/
// 7 - TODO
def countPrimes(n: BigInt): Int = {
  val range = BigInt(1) to n
  range.count(_.isProbablePrime(100))
}
countPrimes(1000)

// 8
