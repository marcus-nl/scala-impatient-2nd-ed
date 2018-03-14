// chapter 20
import scala.util.parsing.combinator.RegexParsers

def expect(actual: Int, expected: Int) {
  assert(actual == expected, s"$actual != $expected")
}

// 1 + 2
class ExprParser extends RegexParsers {
  val number = "[0-9]+".r

  def expr: Parser[Int] = term ~ rep(("+" | "-") ~ term) ^^ {
    case t ~ rep => rep.foldLeft(t)((acc, r) => r match {
      case "+" ~ t => acc + t
      case "-" ~ t => acc - t
    })
  }

  def term: Parser[Int] = factor ~ rep(("*"|"/"|"%") ~ factor) ^^ {
    case f ~ rep => rep.foldLeft(f)((acc, r) => r match {
      case "*" ~ f => acc * f
      case "/" ~ f => acc / f
      case "%" ~ f => acc % f
    })
  }

  def factor: Parser[Int] = primary ~ opt("^" ~ expr) ^^ {
    case p ~ None => p
    case p ~ Some("^" ~ e) => Math.pow(p, e).toInt
  }

  def primary: Parser[Int] = {
    number ^^ { _.toInt } |
    "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _  => e }
  }
}

val parser = new ExprParser
def test(input: String, expected: Int): Unit = {
  val result = parser.parseAll(parser.expr, input)
  assert(result.successful, s"parse result of $input not succesful")
  val actual = result.get
  assert(actual == expected, s"parse result of $input should be $expected, but is $actual")
  println(s"Ok: $input -> $actual")
}

test("1", 1)
test("1+2", 1+2)
test("3-4-5", 3-4-5)
test("3-4*2+5", 3-4*2+5)
test("3*4/2*5", 3*4/2*5)
test("(4^2)^3", Math.pow(Math.pow(4, 2), 3).toInt )
test("4^2^3", Math.pow(4, Math.pow(2, 3)).toInt )

// 3
class IntListParser extends RegexParsers {
  val number = "-?[0-9]+".r

  def list: Parser[List[Int]] = "(" ~> repsep(literal, ",") <~ ")"

  def literal: Parser[Int] = number ^^ { _.toInt }
}

val parser2 = new IntListParser
parser2.parseAll(parser2.list, "()")
parser2.parseAll(parser2.list, "(5)")
parser2.parseAll(parser2.list, "(1, 23, -79)")

// 4
import java.time._

class ISO8601Parser extends RegexParsers {
  val num = "[0-9]+".r

  def date: Parser[LocalDate] = num ~ "-" ~ num ~ "-" ~ num ^^ {
    case y ~ _ ~ m ~ _ ~ d => LocalDate.of(y.toInt, m.toInt, d.toInt)
  }

  def time: Parser[LocalTime] = num ~ ":" ~ num ~ ":" ~ num ^^ {
    case h ~ _ ~ m ~ _ ~ s => LocalTime.of(h.toInt, m.toInt, s.toInt)
  }

  def datetime: Parser[LocalDateTime] = date ~ "T" ~ time ^^ {
    case d ~ _ ~ t => d.atTime(t)
  }
}

val dtParser = new ISO8601Parser
dtParser.parseAll(dtParser.date, "2018-03-12")
dtParser.parseAll(dtParser.time, "15:54:22")
dtParser.parseAll(dtParser.datetime, "2018-03-12T15:54:22")

// 5
import scala.xml._
type XMLElem = scala.xml.Elem

class TagParser extends RegexParsers {
  val name = "\\w+".r
  val value = "[^'\"]*".r

  def elem: Parser[XMLElem] = emptyTag |
    startTag >> { start =>
      rep(elem) <~ endTag(start._1) ^^ {
        children => createElem(start._1, start._2, children)
      }
    }

  def emptyTag: Parser[XMLElem] = "<" ~> name ~ attributes <~ "/>" ^^ {
    case name ~ attrs => createElem(name, attrs, Nil)
  }

  def startTag: Parser[(String,MetaData)] = "<" ~> name ~ attributes <~ ">" ^^ {
    case name ~ attrs => (name, attrs)
  }

  def endTag(name: String): Parser[String] = "</" ~> name <~ ">"

  def attributes: Parser[MetaData] = rep(attribute) ^^ {
    attrs => {
      val initial: MetaData = Null;
      attrs.reverse.foldLeft(initial)((next, attr) => createAtt(attr._1, attr._2, next))
    }
  }

  def attribute: Parser[(String,String)] = (name <~ "=") ~ attributeValue ^^ {
    case name ~ value => (name, value)
  }

  def attributeValue: Parser[String] = "\"" ~> value <~ "\"" | "'" ~> value <~ "'"

  def createElem(name: String, attrs: MetaData, children: List[Node]) =
    new XMLElem(null, name, attrs, TopScope, true, children : _*)

  def createAtt(name: String, value: String, next: MetaData) =
    new UnprefixedAttribute(name, value, next)
}

val tagParser = new TagParser
tagParser.parseAll(tagParser.elem, """<foo bar="quux" hello='world'/>""")
tagParser.parseAll(tagParser.elem, """<foo></unmatched>""")
tagParser.parseAll(tagParser.elem,
  """<foo bar="quux" hello='world'>
    |<inner/><another with='attribute'/></foo>""".stripMargin)

// 6
import scala.util.parsing.combinator.RegexParsers

class Expr
case class Number(value: Int) extends Expr
case class Operator(op: String, left: Expr, right: Expr) extends Expr

class ExprTreeParser extends RegexParsers {
  val number = "[0-9]+".r

  def expr: Parser[Expr] = term ~ rep(("+" | "-") ~ term) ^^ {
    case t ~ rep => rep.foldLeft(t)((acc, r) => r match {
      case op ~ t => Operator(op, acc, t)
    })
  }

  def term: Parser[Expr] = factor ~ rep(("*"|"/"|"%") ~ factor) ^^ {
    case f ~ rep => rep.foldLeft(f)((acc, r) => r match {
      case op ~ f => Operator(op, acc, f)
    })
  }

  def factor: Parser[Expr] = primary ~ opt("^" ~ expr) ^^ {
    case p ~ None => p
    case p ~ Some(op ~ e) => Operator(op, p, e)
  }

  def primary: Parser[Expr] = {
    number ^^ { n => Number(n.toInt) } |
      "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _  => e }
  }
}

val etp = new ExprTreeParser
etp.parseAll(etp.expr, "1")
etp.parseAll(etp.expr, "1 + 2")
etp.parseAll(etp.expr, "3 - 4 - 5")
etp.parseAll(etp.expr, "3 - 4 * 2 + 5")
etp.parseAll(etp.expr, "3 + 4 * 5")
etp.parseAll(etp.expr, "(4 ^ 2) ^ 3")
etp.parseAll(etp.expr, "4 ^ 2 ^ 3")

// 7 - oops, already did that
