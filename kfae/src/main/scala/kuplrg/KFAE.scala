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
  // first-class continuations
  case Vcc(name: String, body: Expr)

  // the string form of an expression
  def str: String = this match
    case Num(n)    => n.toString
    case Add(l, r) => s"(${l.str} + ${r.str})"
    case Mul(l, r) => s"(${l.str} * ${r.str})"
    case Id(x)     => x
    case Fun(p, b) => s"{ $p => ${b.str} }"
    case App(f, e) => s"${f.str}(${e.str})"
    case Vcc(x, b) => s"{ vcc $x; ${b.str} }"

// environments
type Env = Map[String, Value]

// continuations
enum Cont:
  case EmptyK
  case EvalK(env: Env, expr: Expr, k: Cont)
  case AddK(k: Cont)
  case MulK(k: Cont)
  case AppK(k: Cont)

  // the string form of an expression
  def str: String = this match
    case EmptyK => "[]"
    case EvalK(_, expr, k) => s"(_ |- ${expr.str}) :: ${k.str}"
    case AddK(k) => s"(+) :: ${k.str}"
    case MulK(k) => s"(x) :: ${k.str}"
    case AppK(k) => s"(@) :: ${k.str}"

// value stacks
type Stack = List[Value]

// values
enum Value:
  // number values
  case NumV(number: BigInt)
  // closure values
  case CloV(param: String, body: Expr, env: Env)
  // continuation values
  case ContV(cont: Cont, stack: Stack)

  // the string form of a value
  def str: String = this match
    case NumV(n)       => n.toString
    case CloV(p, b, e) => "<function>"
    case ContV(k, s)   => "<continuation>"

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
  private val keywords = Set("val", "vcc")
  private val d: String = "0-9"
  private val w: String = "_a-zA-Z"
  private lazy val num: P[BigInt] = s"-?[$d]+".r ^^ BigInt.apply
  private lazy val id: P[String] = regex(s"[$w][$w$d]*".r)
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
      ("vcc" ~> id <~ ";") ~ e0 ^^ {
        case x ~ b => Vcc(x, b)
      } |
      ("val" ~> id <~ "=") ~ e1 ~ (";" ~> e0) ^^ {
        case x ~ i ~ b => Val(x, i, b)
      } |
      (id <~ "=>") ~ e0 ^^ { case p ~ b => Fun(p, b) } |
      id ^^ Id.apply
    )
    e0

  // desugaring rules
  def Val(x: String, expr: Expr, body: Expr): Expr = App(Fun(x, body), expr)
}
