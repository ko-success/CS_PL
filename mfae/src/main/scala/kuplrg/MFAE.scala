package kuplrg

import scala.util.parsing.combinator.*

// expressions
enum Expr:
  // number
  case Num(number: BigInt)
  // addition
  case Add(left: Expr, right: Expr)
  // multiplication
  case Mul(left: Expr, right: Expr)
  // mutable variable definition
  case Var(name: String, init: Expr, body: Expr)
  // identifier lookup
  case Id(name: String)
  // anonymous (lambda) function
  case Fun(param: String, body: Expr)
  // function application
  case App(fun: Expr, arg: Expr)
  // variable assignment
  case Assign(name: String, expr: Expr)
  // sequence
  case Seq(left: Expr, right: Expr)

  // the string form of an expression
  def str: String = this match
    case Num(n)       => n.toString
    case Add(l, r)    => s"(${l.str} + ${r.str})"
    case Mul(l, r)    => s"(${l.str} * ${r.str})"
    case Var(x, i, b) => s"{ var $x = ${i.str}; ${b.str} }"
    case Id(x)        => x
    case Fun(p, b)    => s"{ $p => ${b.str} }"
    case App(f, a)    => s"${f.str}(${a.str})"
    case Assign(x, e) => s"{ $x = ${e.str} }"
    case Seq(l, r)    => s"{ ${l.str}; ${r.str} }"

// environments
type Env = Map[String, Addr]

// addresses
type Addr = Int

// memories
type Mem = Map[Addr, Value]

// values
enum Value:
  // number values
  case NumV(number: BigInt)
  // closure values
  case CloV(param: String, body: Expr, env: Env)

  // the string form of a value
  def str: String = this match
    case NumV(n)       => n.toString
    case CloV(p, b, e) => "<function>"

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
  private val keywords = Set("var")
  private val d: String = "0-9"
  private val w: String = "_a-zA-Z"
  private lazy val num: P[BigInt] = s"-?[$d]+".r ^^ BigInt.apply
  private lazy val id: P[String] = s"[$w][$w$d]*".r.withFilter(!keywords(_))
  lazy val expr: P[Expr] =
    lazy val e0: P[Expr] = rep1sep(e1, ";") ^^ { _.reduceLeft(Seq.apply) }
    lazy val e1: P[Expr] =
      (id <~ "=") ~ e1 ^^ { case x ~ e => Assign(x, e) } |
      (id <~ "+=") ~ e1 ^^ { case x ~ e => AddAssign(x, e) } |
      (id <~ "*=") ~ e1 ^^ { case x ~ e => MulAssign(x, e) } |
      (id <~ "=>") ~ e1 ^^ { case p ~ b => Fun(p, b) } |
      ("var" ~> id <~ "=") ~ e1 ~ (";" ~> e0) ^^ {
        case x ~ i ~ b => Var(x, i, b)
      } |
      e2
    lazy val e2: P[Expr] = rep1sep(e3, "+") ^^ { _.reduceLeft(Add.apply) }
    lazy val e3: P[Expr] = rep1sep(e4, "*") ^^ { _.reduceLeft(Mul.apply) }
    lazy val e4: P[Expr] = e5 ~ rep("(" ~> e0 <~")") ^^ {
      case f ~ as => as.foldLeft(f)(App.apply)
    }
    lazy val e5: P[Expr] = (
      "(" ~> e0 <~ ")" |
      "{" ~> e0 <~ "}" |
      num ^^ Num.apply |
      id ^^ Id.apply
    )
    e0

  // desugaring rules
  def AddAssign(x: String, expr: Expr): Expr = Assign(x, Add(Id(x), expr))
  def MulAssign(x: String, expr: Expr): Expr = Assign(x, Mul(Id(x), expr))
}
