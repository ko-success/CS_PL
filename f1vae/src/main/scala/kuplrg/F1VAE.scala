package kuplrg

import scala.util.parsing.combinator.*

// programs
case class Program(fdefs: List[FunDef], expr: Expr):
  // the string form of a program
  def str: String = fdefs.map(_.str).mkString("\n") + "\n" + expr.str

// function definitions
case class FunDef(name: String, param: String, body: Expr):
  // the string form of a function definition
  def str: String = s"def $name($param) = ${body.str};"

// expressions
enum Expr:
  // numbers
  case Num(number: BigInt)
  // additions
  case Add(left: Expr, right: Expr)
  // multiplications
  case Mul(left: Expr, right: Expr)
  // immutable variable definitions
  case Val(name: String, init: Expr, body: Expr)
  // identifier lookups
  case Id(name: String)
  // function applications
  case App(fname: String, arg: Expr)

  // the string form of an expression
  def str: String = this match
    case Num(n)       => n.toString
    case Add(l, r)    => s"(${l.str} + ${r.str})"
    case Mul(l, r)    => s"(${l.str} * ${r.str})"
    case Val(x, i, b) => s"{ val $x = ${i.str}; ${b.str} }"
    case Id(x)        => x
    case App(f, e)    => s"$f(${e.str})"

// values
type Value = BigInt

// environments
type Env = Map[String, Value]

// function environments
type FEnv = Map[String, FunDef]

// -----------------------------------------------------------------------------
// Parsers
// -----------------------------------------------------------------------------
object Program extends Parser.From(Parser.prog)
object FunDef extends Parser.From(Parser.fdef)
object Expr extends Parser.From(Parser.expr)
object Parser extends RegexParsers with PackratParsers {
  type P[+T] = PackratParser[T]
  class From[T](p: Parser[T]) {
    def apply(s: String): T = parseAll(p, s).getOrElse(error("parse error"))
  }
  private val keywords = Set("val", "def")
  private val d: String = "0-9"
  private val w: String = "_a-zA-Z"
  private lazy val num: P[BigInt] = s"-?[$d]+".r ^^ BigInt.apply
  private lazy val id: P[String] = s"[$w][$w$d]*".r.withFilter(!keywords(_))
  lazy val prog: P[Program] =
    rep(fdef) ~ expr ^^ { case fs ~ e => Program(fs, e) }
  lazy val fdef: P[FunDef] =
    ("def" ~> id) ~ ("(" ~> id <~ ")") ~ ("=" ~> expr <~ ";") ^^ {
      case x ~ p ~ b => FunDef(x, p, b)
    }
  lazy val expr: P[Expr] =
    import Expr.*
    lazy val e0: P[Expr] = rep1sep(e1, "+") ^^ { _.reduceLeft(Add.apply) }
    lazy val e1: P[Expr] = rep1sep(e2, "*") ^^ { _.reduceLeft(Mul.apply) }
    lazy val e2: P[Expr] = (
      "(" ~> e0 <~ ")" |
      "{" ~> e0 <~ "}" |
      num ^^ Num.apply |
      ("val" ~> id <~ "=") ~ e0 ~ (";" ~> e0) ^^ {
        case x ~ i ~ b => Val(x, i, b)
      } |
      id ~ ("(" ~> e0 <~ ")") ^^ { case f ~ a => App(f, a) } |
      id ^^ Id.apply
    )
    e0
}
