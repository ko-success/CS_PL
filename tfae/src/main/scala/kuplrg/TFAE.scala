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
  // immutable variable definition
  case Val(name: String, init: Expr, body: Expr)
  // identifier lookups
  case Id(name: String)
  // anonymous (lambda) functions
  case Fun(param: String, ty: Type, body: Expr)
  // function applications
  case App(fun: Expr, arg: Expr)

  // the string form of an expression
  def str: String = this match
    case Num(n)       => n.toString
    case Add(l, r)    => s"(${l.str} + ${r.str})"
    case Mul(l, r)    => s"(${l.str} * ${r.str})"
    case Val(x, i, b) => s"{ val $x = ${i.str}; ${b.str} }"
    case Id(x)        => x
    case Fun(p, t, b) => s"{ ($p: ${t.str}) => ${b.str} }"
    case App(f, e)    => s"${f.str}(${e.str})"

// environments
type Env = Map[String, Value]

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

// type environments
type TypeEnv = Map[String, Type]

// types
enum Type:
  // number types
  case NumT
  // function (arrow) types
  case ArrowT(paramTy: Type, retTy: Type)

  // the string form of a type
  def str: String = this match
    case NumT => "Number"
    case ArrowT(p: ArrowT, b) => s"(${p.str}) => ${b.str}"
    case ArrowT(p, b) => s"${p.str} => ${b.str}"

// -----------------------------------------------------------------------------
// Parsers
// -----------------------------------------------------------------------------
object Expr extends Parser.From(Parser.expr)
object Parser extends RegexParsers with PackratParsers {
  import Expr.*
  import Type.*
  type P[+T] = PackratParser[T]
  class From[T](p: Parser[T]) {
    def apply(s: String): T = parseAll(p, s).getOrElse(error("parse error"))
  }

  private val keywords = Set("val", "Number")
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
      ("(" ~> id <~ ":") ~ ty ~ (")" ~ "=>" ~> e0) ^^ {
        case p ~ t ~ b => Fun(p, t, b)
      } |
      "(" ~> e0 <~ ")" |
      "{" ~> e0 <~ "}" |
      num ^^ Num.apply |
      ("val" ~> id) ~ ("=" ~> e0) ~ (";" ~> e0) ^^ {
        case x ~ i ~ b => Val(x, i, b)
      } |
      id ^^ Id.apply
    )
    e0
  lazy val ty: P[Type] =
    lazy val t0: P[Type] = t1 ~ ("=>" ~> t0) ^^ {
      case p ~ b => ArrowT(p, b)
    } | t1
    lazy val t1: P[Type] = (
      "(" ~> t0 <~ ")" |
      "Number" ^^^ NumT
    )
    t0
}
