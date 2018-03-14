// 9: 8 with operators "and", "or" and the if-else expression (not statement).
// skipped while, since that cannot be expressed as an expression..
import scala.collection.mutable
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

def and(a: Int, b: Int): Int = if (a == 1 && b == 1) 1 else 0
def or(a: Int, b: Int):  Int = if (a == 1 || b == 1) 1 else 0

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
    case "or"   => or(left.eval(ctx),  right.eval(ctx))
    case "and"  => and(left.eval(ctx), right.eval(ctx))
    case "+"    => left.eval(ctx) + right.eval(ctx)
    case "-"    => left.eval(ctx) - right.eval(ctx)
    case "*"    => left.eval(ctx) * right.eval(ctx)
    case "/"    => left.eval(ctx) / right.eval(ctx)
    case "%"    => left.eval(ctx) % right.eval(ctx)
    case "^"    => Math.pow(left.eval(ctx), right.eval(ctx)).toInt
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
case class If(cond: Expr, trueExpr: Expr, falseExpr: Expr) extends Expr {
  override def eval(ctx: Context): Int =
    if (cond.eval(ctx) != 0) trueExpr.eval(ctx) else falseExpr.eval(ctx)
}

class ExprTreeParser extends StandardTokenParsers {
  lexical.reserved += ("and", "or", "if", "then", "else")
  lexical.delimiters += ("+", "-", "*", "/", "%", "^", "=", "(", ")")

  // left-associative binary expression
  def binOpLeft(op: Parser[String], next: Parser[Expr]): Parser[Expr] =
    next ~ rep(op ~ next) ^^ {
      case i ~ rep => rep.foldLeft(i)((acc, r) => r match {
        case op ~ t => Operator(op, acc, t)
      })
    }

  // right-associative binary expression
  def binOpRight(op: Parser[String], next: Parser[Expr]): Parser[Expr] =
    next ~ opt(op ~ expr) ^^ {
      case l ~ None => l
      case l ~ Some(op ~ r) => Operator(op, l, r)
    }

  def expr = orExpr

  def orExpr    = binOpLeft("or", andExpr)
  def andExpr   = binOpLeft("and", addExpr)
  def addExpr   = binOpLeft(("+" | "-"), multExpr)
  def multExpr  = binOpLeft(("*"|"/"|"%"), powExpr)
  def powExpr   = binOpRight("^", primary)

  def primary: Parser[Expr] = {
    ifExpr |
    ident ~ "=" ~ expr  ^^ { case id ~ "=" ~ e => Assign(id, e) } |
    ident               ^^ { id => new Ref(id) } |
    numericLit          ^^ { n => Number(n.toInt) } |
    "(" ~ expr ~ ")"    ^^ { case "(" ~ e ~ ")"  => e }
  }

  def ifExpr: Parser[Expr] = "if" ~ expr ~ "then" ~ expr ~ "else" ~ expr ^^ {
    case "if" ~ c ~ "then" ~ t ~ "else" ~ f => If(c, t, f)
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
test("1 and 1", 1)
test("1 and 1 or 0", 1)
test("1 + if 1 and 0 then 2 else 3", 1 + 3)
