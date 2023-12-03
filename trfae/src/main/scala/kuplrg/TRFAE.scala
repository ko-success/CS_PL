package kuplrg

import scala.util.parsing.combinator.*

// expressions
enum Expr:
  // numbers
  case Num(number: BigInt)
  // booleans
  case Bool(bool: Boolean)
  // addition
  case Add(left: Expr, right: Expr)
  // multiplication
  case Mul(left: Expr, right: Expr)
  // division
  case Div(left: Expr, right: Expr)
  // modulo
  case Mod(left: Expr, right: Expr)
  // equal-to
  case Eq(left: Expr, right: Expr)
  // less-than
  case Lt(left: Expr, right: Expr)
  // immutable variable definition
  case Val(name: String, init: Expr, body: Expr)
  // identifier lookups
  case Id(name: String)
  // anonymous (lambda) functions
  case Fun(p: String, pty: Type, body: Expr)
  // recursive functions
  case Rec(x: String, p: String, pty: Type, rty: Type, body: Expr, scope: Expr)
  // function applications
  case App(fun: Expr, arg: Expr)
  // conditional
  case If(cond: Expr, thenExpr: Expr, elseExpr: Expr)

  // the string form of an expression
  def str: String = this match
    case Num(n)       => n.toString
    case Bool(b)      => b.toString
    case Add(l, r)    => s"(${l.str} + ${r.str})"
    case Mul(l, r)    => s"(${l.str} * ${r.str})"
    case Div(l, r)    => s"(${l.str} / ${r.str})"
    case Mod(l, r)    => s"(${l.str} % ${r.str})"
    case Eq(l, r)     => s"(${l.str} == ${r.str})"
    case Lt(l, r)     => s"(${l.str} < ${r.str})"
    case Val(x, i, b) => s"{ val $x = ${i.str}; ${b.str} }"
    case Id(x)        => x
    case Fun(p, t, b) => s"{ ($p: ${t.str}) => ${b.str} }"
    case App(f, e)    => s"${f.str}(${e.str})"
    case If(c, t, e)  => s"if (${c.str}) ${t.str} else ${e.str}"
    case Rec(n, p, pt, rt, b, s) =>
      s"{ def $n($p: ${pt.str}): ${rt.str} = ${b.str}; ${s.str} }"

// environments
type Env = Map[String, Value]

// values
enum Value:
  // number values
  case NumV(number: BigInt)
  // boolean values
  case BoolV(bool: Boolean)
  // closure values
  case CloV(param: String, body: Expr, env: () => Env)

  // the string form of a value
  def str: String = this match
    case NumV(n)       => n.toString
    case BoolV(b)      => b.toString
    case CloV(p, b, e) => "<function>"

// type environments
type TypeEnv = Map[String, Type]

// types
enum Type:
  // number types
  case NumT
  // boolean types
  case BoolT
  // function (arrow) types
  case ArrowT(paramTy: Type, retTy: Type)

  // the string form of a type
  def str: String = this match
    case NumT => "Number"
    case BoolT => "Boolean"
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
  private val keywords = Set(
    "true", "false", "def", "if", "else", "val", "Number", "Boolean"
  )
  private val d: String = "0-9"
  private val w: String = "_a-zA-Z"
  private lazy val num: P[BigInt] = s"-?[$d]+".r ^^ BigInt.apply
  private lazy val bool: P[Boolean] = "true" ^^^ true | "false" ^^^ false
  private lazy val id: P[String] = s"[$w][$w$d]*".r.withFilter(!keywords(_))

  // expressions
  lazy val expr: P[Expr] =
    lazy val e0: P[Expr] = rep1sep(e1, "||") ^^ (_.reduceLeft(Or))
    lazy val e1: P[Expr] = rep1sep(e2, "&&") ^^ (_.reduceLeft(And))
    lazy val e2: P[Expr] = e3 ~ rep(("==" | "!=") ~ e3) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (l, "==" ~ r) => Eq(l, r)
        case (l, _ ~ r) => Ne(l, r)
      }
    }
    lazy val e3: P[Expr] = e4 ~ rep(("<=" | "<" | ">=" | ">") ~ e4) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (l, "<" ~ r)  => Lt(l, r)
        case (l, "<=" ~ r) => Le(l, r)
        case (l, ">" ~ r)  => Gt(l, r)
        case (l, _ ~ r) => Ge(l, r)
      }
    }
    lazy val e4: P[Expr] = e5 ~ rep(("+" | "-") ~ e5) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (l, "+" ~ r) => Add(l, r)
        case (l, _ ~ r) => Sub(l, r)
      }
    }
    lazy val e5: P[Expr] = e6 ~ rep(("*" | "/" | "%") ~ e6) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (l, "*" ~ r) => Mul(l, r)
        case (l, "/" ~ r) => Div(l, r)
        case (l, _ ~ r) => Mod(l, r)
      }
    }
    lazy val e6: P[Expr] = "-" ~> e6 ^^ Neg | "!" ~> e6 ^^ Not | e7
    lazy val e7: P[Expr] = e8 ~ rep("(" ~> e0 <~")") ^^ {
      case f ~ as => as.foldLeft(f)(App.apply)
    }
    lazy val e8: P[Expr] = (
      ("(" ~> id ~ (":" ~> ty) <~ ")") ~ ("=>" ~> e0) ^^ {
        case p ~ t ~ b => Fun(p, t, b)
      } |
      "(" ~> e0 <~ ")" |
      "{" ~> e0 <~ "}" |
      num ^^ Num.apply |
      bool ^^ Bool.apply |
      ("val" ~> id) ~ ("=" ~> e0) ~ (";" ~> e0) ^^ {
        case x ~ i ~ b => Val(x, i, b)
      } |
      "def" ~> id ~ ("(" ~> id ~ (":" ~> ty) <~ ")") ~ (":" ~> ty) ~
      ("=" ~> e0 <~ ";") ~ e0 ^^ {
        case x ~ (p ~ pt) ~ rt ~ b ~ s => Rec(x, p, pt, rt, b, s)
      } |
      "if" ~> ("(" ~> e0 <~ ")") ~ e0 ~ ("else" ~> e0) ^^ {
        case c ~ t ~ e => If(c, t, e)
      } |
      id ^^ Id.apply
    )
    e0

  // types
  private lazy val ty: P[Type] =
    lazy val t0: P[Type] = t1 ~ ("=>" ~> t0) ^^ {
      case p ~ b => ArrowT(p, b)
    } | t1
    lazy val t1: P[Type] = (
      "(" ~> t0 <~ ")" |
      "Number" ^^^ NumT |
      "Boolean" ^^^ BoolT
    )
    t0

  // desugaring rules
  val T: Expr = Bool(true)
  val F: Expr = Bool(false)
  def Neg(expr: Expr): Expr = Mul(expr, Num(-1))
  def Not(expr: Expr): Expr = If(expr, F, T)
  def Sub(left: Expr, right: Expr): Expr = Add(left, Neg(right))
  def And(left: Expr, right: Expr): Expr = If(left, right, F)
  def Or(left: Expr, right: Expr): Expr = If(left, T, right)
  def Ne(left: Expr, right: Expr): Expr = Not(Eq(left, right))
  def Le(left: Expr, right: Expr): Expr = Or(Lt(left, right), Eq(left, right))
  def Gt(left: Expr, right: Expr): Expr = Not(Le(left, right))
  def Ge(left: Expr, right: Expr): Expr = Not(Lt(left, right))
}
