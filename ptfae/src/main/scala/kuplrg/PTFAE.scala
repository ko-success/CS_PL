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
  // type abstraction
  case TypeAbs(name: String, body: Expr)
  // type application
  case TypeApp(expr: Expr, ty: Type)

  // the string form of an expression
  def str: String = this match
    case Num(n)        => n.toString
    case Add(l, r)     => s"(${l.str} + ${r.str})"
    case Mul(l, r)     => s"(${l.str} * ${r.str})"
    case Val(x, i, b)  => s"{ val $x = ${i.str}; ${b.str} }"
    case Id(x)         => x
    case Fun(p, t, b)  => s"{ ($p: ${t.str}) => ${b.str} }"
    case App(f, e)     => s"${f.str}(${e.str})"
    case TypeAbs(p, b) => s"forall[$p] { ${b.str} }"
    case TypeApp(e, t) => s"${e.str}[${t.str}]"

// environments
type Env = Map[String, Value]

// values
enum Value:
  // number values
  case NumV(number: BigInt)
  // closure values
  case CloV(param: String, body: Expr, env: Env)
  // type abstraction values
  case TypeAbsV(name: String, body: Expr, env: Env)

  // the string form of a value
  def str: String = this match
    case NumV(n)           => n.toString
    case CloV(p, b, e)     => "<function>"
    case TypeAbsV(p, b, e) => "<type-abs>"

// type environments
case class TypeEnv(
  vars: Map[String, Type] = Map(),
  tys: Set[String] = Set(),
) {
  def addVar(pair: (String, Type)): TypeEnv =
    TypeEnv(vars + pair, tys)
  def addVars(pairs: Iterable[(String, Type)]): TypeEnv =
    TypeEnv(vars ++ pairs, tys)
  def addType(name: String): TypeEnv = TypeEnv(vars, tys + name)
}

// types
enum Type:
  // number types
  case NumT
  // function (arrow) types
  case ArrowT(paramTy: Type, retTy: Type)
  // type variables
  case VarT(name: String)
  // polymorphic types (parametric polymorphism)
  case PolyT(name: String, ty: Type)

  // the string form of a type
  def str: String = this match
    case NumT                            => "Number"
    case ArrowT(p @ (NumT | VarT(_)), b) => s"${p.str} => ${b.str}"
    case ArrowT(p, b)                    => s"(${p.str}) => ${b.str}"
    case VarT(x)                         => x
    case PolyT(x, t: PolyT)              => s"[$x]${t.str}"
    case PolyT(x, t)                     => s"[$x](${t.str})"

// -----------------------------------------------------------------------------
// Parsers
// -----------------------------------------------------------------------------
object Expr extends Parser.From(Parser.expr)
object Type extends Parser.From(Parser.ty)
object Parser extends RegexParsers with PackratParsers {
  import Expr.*
  import Type.*
  type P[+T] = PackratParser[T]
  class From[T](p: Parser[T]) {
    def apply(s: String): T = parseAll(p, s).getOrElse(error("parse error"))
  }

  private val keywords = Set("val", "Number", "forall")
  private val d: String = "0-9"
  private val w: String = "_a-zA-Z"
  private lazy val num: P[BigInt] = s"-?[$d]+".r ^^ BigInt.apply
  private lazy val id: P[String] = s"[$w][$w$d]*".r.withFilter(!keywords(_))
  lazy val expr: P[Expr] =
    import PostfixOps.*
    lazy val e0: P[Expr] = rep1sep(e1, "+") ^^ { _.reduceLeft(Add.apply) }
    lazy val e1: P[Expr] = rep1sep(e2, "*") ^^ { _.reduceLeft(Mul.apply) }
    lazy val e2: P[Expr] = e3 ~ rep(
      "(" ~> e0 <~ ")" ^^ PApp.apply |
      "[" ~> ty <~ "]" ^^ PTypeApp.apply
    ) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (f, PApp(a)) => App(f, a)
        case (e, PTypeApp(t)) => TypeApp(e, t)
      }
    }

    lazy val e3: P[Expr] = (
      ("(" ~> id <~ ":") ~ ty ~ (")" ~ "=>" ~> e0) ^^ {
        case p ~ t ~ b => Fun(p, t, b)
      } |
      ("forall" ~ "[" ~> id <~ "]") ~ e0 ^^ {
        case p ~ b => TypeAbs(p, b)
      } |
      "(" ~> e0 <~ ")" |
      "{" ~> e0 <~ "}" |
      num ^^ Num.apply |
      ("val" ~> id) ~ ("=" ~> e0) ~ (opt(";") ~> e0) ^^ {
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
      "Number" ^^^ NumT |
      ("[" ~> id <~ "]") ~ t0 ^^ { case p ~ b => PolyT(p, b) } |
      id ^^ VarT.apply
    )
    t0

  // postfix operators
  private enum PostfixOps:
    case PApp(arg: Expr)
    case PTypeApp(ty: Type)
}
