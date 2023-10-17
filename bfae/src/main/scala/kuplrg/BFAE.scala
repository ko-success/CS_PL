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
  // identifier lookup
  case Id(name: String)
  // anonymous (lambda) function
  case Fun(param: String, body: Expr)
  // function application
  case App(fun: Expr, arg: Expr)
  // box creation
  case NewBox(content: Expr)
  // box content getter
  case GetBox(box: Expr)
  // box content setter
  case SetBox(box: Expr, content: Expr)
  // sequence
  case Seq(left: Expr, right: Expr)

  // the string form of an expression
  def str: String = this match
    case Num(n)       => n.toString
    case Add(l, r)    => s"(${l.str} + ${r.str})"
    case Mul(l, r)    => s"(${l.str} * ${r.str})"
    case Id(x)        => x
    case Fun(p, b)    => s"{ $p => ${b.str} }"
    case App(f, a)    => s"${f.str}(${a.str})"
    case NewBox(c)    => s"Box(${c.str})"
    case GetBox(b)   => s"${b.str}.get"
    case SetBox(b, c) => s"${b.str}.set(${c.str})"
    case Seq(l, r)    => s"{ ${l.str}; ${r.str} }"

// environments
type Env = Map[String, Value]

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
  // box values
  case BoxV(addr: Addr)

  // the string form of a value
  def str: String = this match
    case NumV(n)       => n.toString
    case CloV(p, b, e) => "<function>"
    case BoxV(a)       => s"<box>"

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
    import PostfixOps.*
    lazy val e0: P[Expr] = rep1sep(e1, ";") ^^ { _.reduceLeft(Seq.apply) }
    lazy val e1: P[Expr] = rep1sep(e2, "+") ^^ { _.reduceLeft(Add.apply) }
    lazy val e2: P[Expr] = rep1sep(e3, "*") ^^ { _.reduceLeft(Mul.apply) }
    lazy val e3: P[Expr] = e4 ~ rep(
      "(" ~> e0 <~ ")" ^^ PApp.apply |
      "." ~> "get" ^^^ PGetBox |
      "." ~> "set" ~> "(" ~> e0 <~ ")" ^^ PSetBox.apply
    ) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (f, PApp(a)) => App(f, a)
        case (b, PGetBox) => GetBox(b)
        case (b, PSetBox(e)) => SetBox(b, e)
      }
    }
    lazy val e4: P[Expr] = (
      "(" ~> e0 <~ ")" |
      "{" ~> e0 <~ "}" |
      num ^^ Num.apply |
      "Box" ~> "(" ~> e0 <~ ")" ^^ NewBox.apply |
      ("val" ~> id <~ "=") ~ e1 ~ (";" ~> e0) ^^ {
        case x ~ i ~ b => Val(x, i, b)
      } |
      (id <~ "=>") ~ e1 ^^ { case p ~ b => Fun(p, b) } |
      id ^^ Id.apply
    )
    e0

  // postfix operators
  private enum PostfixOps:
    case PApp(arg: Expr)
    case PGetBox
    case PSetBox(content: Expr)

  // desugaring rules
  def Val(x: String, expr: Expr, body: Expr): Expr = App(Fun(x, body), expr)
}
