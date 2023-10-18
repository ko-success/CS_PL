package kuplrg

import scala.util.parsing.combinator.*

// expressions
enum Expr:
  // numbers
  case Num(number: BigInt)
  // additions
  case Add(left: Expr, right: Expr)
  // multiplications
  case Mul(left: Expr, right: Expr)
  // identifier lookups
  case Id(name: String)
  // anonymous (lambda) functions
  case Fun(param: String, body: Expr)
  // function applications
  case App(fun: Expr, arg: Expr)

  // the string form of an expression
  def str: String = this match
    case Num(n)       => n.toString
    case Add(l, r)    => s"(${l.str} + ${r.str})"
    case Mul(l, r)    => s"(${l.str} * ${r.str})"
    case Id(x)        => x
    case Fun(p, b)    => s"{ $p => ${b.str} }"
    case App(f, e)    => s"${f.str}(${e.str})"

// environments
type Env = Map[String, Value]

// values
enum Value:
  // number values
  case NumV(number: BigInt)
  // closure values
  case CloV(param: String, body: Expr, env: Env)
  // expression values
  case ExprV(expr: Expr, env: Env)

  // the string form of a value
  def str: String = this match
    case NumV(n)       => n.toString
    case CloV(_, _, _) => "<function>"
    case ExprV(_, _)   => "<expr>"

// -----------------------------------------------------------------------------
// Parsers
// -----------------------------------------------------------------------------
object Expr extends Parser.From(Parser.expr)
object Parser extends RegexParsers with PackratParsers {
  import Expr.*
  type P[+T] = PackratParser[T]
  class From[T](p: Parser[T]) {
    def apply(s: String): T = parseAll(p, s).getOrElse(error("parse error"))
  }
  private val keywords = Set("Box", "val")
  private val d: String = "0-9"
  private val w: String = "_a-zA-Z"
  private lazy val num: P[BigInt] = s"-?[$d]+".r ^^ BigInt.apply
  private lazy val id: P[String] = s"[$w][$w$d]*".r.withFilter(!keywords(_))
  lazy val expr: P[Expr] =
    lazy val e0: P[Expr] = rep1sep(e1, "+") ^^ { _.reduceLeft(Add.apply) }
    lazy val e1: P[Expr] = rep1sep(e2, "*") ^^ { _.reduceLeft(Mul.apply) }
    lazy val e2: P[Expr] = e3 ~ rep("(" ~> e0 <~")") ^^ {
      case f ~ as => as.foldLeft(f)(App.apply)
    }
    lazy val e3: P[Expr] = (
      "(" ~> e0 <~ ")" |
      "{" ~> e0 <~ "}" |
      num ^^ Num.apply |
      ("val" ~> id <~ "=") ~ e0 ~ (";" ~> e0) ^^ {
        case x ~ i ~ b => Val(x, i, b)
      } |
      (id <~ "=>") ~ e0 ^^ { case p ~ b => Fun(p, b) } |
      id ^^ Id.apply
    )
    e0

  // desugaring rules
  def Val(x: String, expr: Expr, body: Expr): Expr = App(Fun(x, body), expr)
}
