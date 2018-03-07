// chapter 11 - continued

// 6
class AsciiArt(private val lines: Array[String]) {
  override def toString = '\n' + lines.mkString("\n")
  val width = lines.map(_.length).max
  val height = lines.size
  def | (that: AsciiArt) = {
    new AsciiArt(
      for ((a, b) <- this.lines.zipAll(that.lines, " ", " "))
        yield a + " " + b
    )
  }
  def ++ (that: AsciiArt) = {
    val maxWidth = Math.max(this.width, that.width)
    val theseLines = this.lines.map(_.padTo(maxWidth, ' '))
    val thoseLines = that.lines.map(_.padTo(maxWidth, ' '))
    new AsciiArt(theseLines ++ thoseLines)
  }
}
object AsciiArt {
  def apply(lines: String*): AsciiArt = new AsciiArt(lines.toArray)
}

val bunny = AsciiArt(
  """ /\_/\ """,
  """( ' ' )""",
  """(  _  )""",
  """ | | | """,
  """(__|__)""")
val quote = AsciiArt(
  """  -----   """,
  """ / Hello \""",
  """<  Scala |""",
  """ \ Coder /""",
  """   -----  """)
bunny | quote
bunny ++ AsciiArt("""   %""") | quote ++ AsciiArt("""   ^^^^^""")
bunny ++ quote | AsciiArt("""blaaat""")

// 7
class BitSequence(var packed: Long) {
  implicit def bool2int(b: Boolean) = if (b) 1 else 0
  def apply(index: Int): Boolean = (packed & (1L << index)) != 0L
  def update(index: Int, value: Boolean): Unit = {
    if (value)  packed |= 1L << index
    else        packed &= ~(1L << index)
  }
  override def toString = packed.toBinaryString
}
val bs = new BitSequence(12345)
bs(0)
bs(1)
bs(2)
bs(3)
bs(1) = true
bs
bs(2) = false
bs
bs(0) = false
bs

// 8
class Matrix(var a: Int, var b: Int, var c: Int, var d: Int) {
  def + (that: Matrix) = {
    new Matrix(this.a + that.a, this.b + that.b, this.c + that.c, this.d + that.d)
  }

  def * (that: Matrix) = {
    new Matrix(
      a * that.a + b * that.c,
      a * that.b + b * that.d,
      c * that.a + d * that.c,
      c * that.b + d * that.d)
  }

  def * (factor: Int) = {
    new Matrix(this.a * factor, this.b * factor, this.c * factor, this.d * factor)
  }

  def apply(row: Int, col: Int): Int = (row, col) match {
    case (0, 0) => a
    case (1, 0) => b
    case (0, 1) => c
    case (1, 1) => d
  }

  def update(row: Int, col: Int, value: Int): Unit = (row, col) match {
    case (0, 0) => a = value
    case (1, 0) => b = value
    case (0, 1) => c = value
    case (1, 1) => d = value
  }

  override def toString = s"Matrix[[$a, $b], [$c, $d]]"
}

val m1 = new Matrix(1, 2, 3, 4)
val m2 = new Matrix(5, 6, 7, 8)
m1 + m2
m1 * 3
m1 * m2
m1(1, 0)
m1(1, 1) = 5
m1

// 9
import java.nio.file.{Path,Paths}

object PathComponents9 {
  def unapply(path: Path): Option[(String, String)] = {
    Some((path.getParent.toString, path.getFileName.toString))
  }
}
val PathComponents9(dir, fn) = Paths.get("/home/cay/readme.txt")

// 10
object PathComponents10 {
  def unapplySeq(path: String): Option[Seq[String]] = {
    Some("/[^/]+".r.findAllIn(path).toSeq)
  }
}
val PathComponents10(a,b,c) = "/foo/bar/quux"
