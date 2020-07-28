// chapter 9

import java.io.PrintWriter
import scala.io.Source

// 1
val fn1 = "inputs/reverse.txt"
val reversedLines = Source.fromFile(fn1).getLines().toBuffer.reverse
val out = new PrintWriter("outputs/reversed.txt")
for (line <- reversedLines) {
  out.println(line)
}
out.close()

// 2
val fn2 = "inputs/tabs.txt"
for (line <- Source.fromFile(fn2).getLines()) {
  val parts = line.split("\t").map(s => s.padTo(s.length + 4 - s.length % 4, ' '))
  println(parts.mkString)
}

// 3
val fn3 = "inputs/longwords.txt"
Source.fromFile(fn3).mkString.split("\\s+").filter(_.length > 12).foreach(println(_))

// 4
val fn4 = "inputs/numbers.txt"
val doubles = Source.fromFile(fn4).mkString.split("\\s+").map(_.toDouble)
println(doubles.sum)
println(doubles.sum / doubles.length)
println(doubles.max)
println(doubles.min)

// 5
for (exp <- 0 to 20) {
  val pow = Math.pow(2.0, exp)
  val rec = 1.0 / pow
  println(s"${pow.toInt.toString.padTo(11, ' ')}$rec")
  //println(f"$pow%8.0f $rec%f");
  //println("%8.0f    %f".format(pow, rec))
}

// 6
val fn6 = "inputs/cpp.txt"
val src = Source.fromFile(fn6).mkString
for (str <- """"[^"]*"""".r.findAllIn(src)) {
  println(str)
}

// 7 - skipped

// 8
val page = Source.fromURL("http://www.visitgreece.gr/en/nature/forests").mkString
for (m <- """<img[^>]+src="([^"]*)"""".r.findAllMatchIn(page)) {
  println(m.group(1))
}

// 9 - seems like IntelliJ doesn't support SAM conversion here?
import java.nio.file._
val dirName = "~/scala/IdeaProjects/first"
val entries = Files.walk(Paths.get(dirName))
try {
  val count = entries.filter(_.endsWith(".class")).count()
  println(s"count=$count")
} finally {
  entries.close()
}

// 10 - skipped
