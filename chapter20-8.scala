// 8
import scala.collection.mutable
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

class Context {
  val vars = new mutable.HashMap[String,Int]()

  def getVariable(name: String): Int =
    vars.getOrElse(name, 0)

  def setVariable(name: String, value: Int): Unit =
    vars.put(name, value)
}

abstract class Expr {
  def eval(ctx: Context): Int
}
case class Number(value: Int) extends Expr {
  override def eval(ctx: Context) = value
}
case class Operator(op: String, left: Expr, right: Expr) extends Expr {
  override def eval(ctx: Context) = op match {
    case "+" => left.eval(ctx) + right.eval(ctx)
    case "-" => left.eval(ctx) - right.eval(ctx)
    case "*" => left.eval(ctx) * right.eval(ctx)
    case "/" => left.eval(ctx) / right.eval(ctx)
    case "%" => left.eval(ctx) % right.eval(ctx)
    case "^" => Math.pow(left.eval(ctx), right.eval(ctx)).toInt
  }
}
case class Ref(name: String) extends Expr {
  override def eval(ctx: Context) = ctx.getVariable(name)
}
case class Assign(name: String, expr: Expr) extends Expr {
  override def eval(ctx: Context) = {
    val value = expr.eval(ctx)
    if (name == "out")
      println(value)
    else
      ctx.setVariable(name, value);
    value
  }
}

class ExprTreeParser extends StandardTokenParsers {
  lexical.delimiters += ("+", "-", "*", "/", "%", "^", "=", "(", ")")

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
    ident ~ "=" ~ expr  ^^ { case id ~ "=" ~ e => Assign(id, e) } |
    ident               ^^ { id => new Ref(id) } |
    numericLit          ^^ { n => Number(n.toInt) } |
    "(" ~ expr ~ ")"    ^^ { case "(" ~ e ~ ")"  => e }
  }

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] =
    phrase(p)(new lexical.Scanner(in))
}

val parser = new ExprTreeParser
val ctx = new Context()

def test(input: String, expected: Int): Unit = {
  val result = parser.parseAll(parser.expr, input)
  assert(result.successful, s"parse result of $input not succesful")
  val expr = result.get
  val actual = expr.eval(ctx)
  assert(actual == expected, s"parse result of $input should be $expected, but is $actual")
  println(s"Ok: $input -> $actual")
}

test("1 + n", 1 + 0)
test("n = 3 - 4 - 5", 3 - 4 - 5) // -6
test("n + 6", -6 + 6)
test("(4 ^ 2) ^ 3", Math.pow(Math.pow(4, 2), 3).toInt)
test("out = 4 ^ 2 ^ 3", Math.pow(4, Math.pow(2, 3)).toInt)
