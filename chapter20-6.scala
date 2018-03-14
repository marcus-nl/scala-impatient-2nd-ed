// 6, using StandardTokenParsers and with eval
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

abstract class Expr {
  def eval: Int
}
case class Number(value: Int) extends Expr {
  override def eval = value
}
case class Operator(op: String, left: Expr, right: Expr) extends Expr {
  override def eval = op match {
    case "+" => left.eval + right.eval
    case "-" => left.eval - right.eval
    case "*" => left.eval * right.eval
    case "/" => left.eval / right.eval
    case "%" => left.eval % right.eval
    case "^" => left.eval ^ right.eval
  }
}

class ExprTreeParser extends StandardTokenParsers {
  lexical.delimiters += ("+", "-", "*", "/", "%", "^", "(", ")")

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
    numericLit ^^ { n => Number(n.toInt) } |
      "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _  => e }
  }

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] =
    phrase(p)(new lexical.Scanner(in))
}

val parser = new ExprTreeParser
def test(input: String, expected: Int): Unit = {
  val result = parser.parseAll(parser.expr, input)
  assert(result.successful, s"parse result of $input not succesful")
  val expr = result.get
  val actual = expr.eval
  assert(actual == expected, s"parse result of $input should be $expected, but is $actual")
  println(s"Ok: $input -> $actual")
}

test("1", 1)
test("1 + 2", 1 + 2)
test("3 - 4 - 5", 3 - 4 - 5)
test("3 - 4 * 2 + 5", 3 - 4 * 2 + 5)
test("3 + 4 * 5", 3 + 4 * 5)
test("(4 ^ 2) ^ 3", Math.pow(Math.pow(4, 2), 3).toInt)
test("4 ^ 2 ^ 3", Math.pow(4, Math.pow(2, 3)).toInt)
