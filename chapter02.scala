// chapter 2
import java.time.LocalDate

// 1
def signum(v: Int) =
  if (v < 0) -1 else if (v > 0) 1 else 0

signum(3)
signum(0)
signum(-143)

// 2: value is (), type is Unit

// 3
var x: Unit = ()
var y: Int = 0
x = y = 1

// 4
for (x <- 10.to(0, -1)) println(x)
for (x <- 10 to(0, -1)) println(x) // ah, this works as well

// 5
def countdown(n: Int) =
  for (x <- n to(0, -1)) println(x)
countdown(10)

// 6
def unipro(s: String) =
  s.map(c => c.toLong).reduce((a,b) => a * b)

unipro("Hello")

// 7, 8: 6 was already a function without a loop

// 9
def uprec(s: String): Long =
  if (s.isEmpty) 1
  else s.head.toLong * uprec(s.tail)
uprec("Hello")

// 10
def exp(x: Int, n: Int): Float =
  if (n == 0) 1
  else if (n % 2 == 0) {
    val y = exp(x, n / 2)
    y * y
  }
  else if (n > 0) x * exp(x, n - 1)
  else 1 / exp(x, -n)

exp(2, 16)
exp(3, 5)
exp(2, -5)
exp(5, 0)

// 11 - need to use REPL mode for this to work in IntelliJ interactive mode
implicit class DateInterpolator(val sc: StringContext) extends AnyVal {
  def date(args: Any*): LocalDate = {
    if (args.size != 3) throw new Exception("3 arguments required")
    def values = args.map(arg => arg.toString.toInt)
    LocalDate.of(values(0), values(1), values(2))
  }
}

def year = 2018
def month = 4
def day = 24
date"$year-$month-$day"
date"${1}-${2}-${3}"
date"${1}-${2}" // -> Exception

