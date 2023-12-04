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
  case Val(name: String, tyOpt: Option[Type], init: Expr, body: Expr)
  // identifier lookups
  case Id(name: String)
  // anonymous (lambda) functions
  case Fun(param: String, ty: Type, body: Expr)
  // function applications
  case App(fun: Expr, arg: Expr)
  // records
  case Record(fields: List[(String, Expr)])
  // field lookups
  case Access(record: Expr, field: String)
  // exit
  case Exit

  // the string form of an expression
  def str: String = this match
    case Num(n)       => n.toString
    case Add(l, r)    => s"(${l.str} + ${r.str})"
    case Mul(l, r)    => s"(${l.str} * ${r.str})"
    case Id(x)        => x
    case Fun(p, t, b) => s"{ ($p: ${t.str}) => ${b.str} }"
    case App(f, e)    => s"${f.str}(${e.str})"
    case Access(r, f) => s"${r.str}.$f"
    case Val(x, None, i, b) =>
      s"{ val $x = ${i.str}; ${b.str} }"
    case Val(x, Some(t), i, b) =>
      s"{ val $x: ${t.str} = ${i.str}; ${b.str} }"
    case Record(fs) =>
      s"{${fs.map { case (f, e) => s"$f=${e.str}" }.mkString(", ")}}"

// environments
type Env = Map[String, Value]

// values
enum Value:
  // number values
  case NumV(number: BigInt)
  // closure values
  case CloV(param: String, body: Expr, env: Env)
  // record values
  case RecordV(fields: Map[String, Value])

  // the string form of a value
  def str: String = this match
    case NumV(n)           => n.toString
    case CloV(p, b, e)     => "<function>"
    case RecordV(fields)   =>
      s"{${fields.map { case (f, v) => s"$f=${v.str}" }.mkString(", ")}}"

// type environments
type TypeEnv = Map[String, Type]

// types
enum Type:
  // number types
  case NumT
  // function (arrow) types
  case ArrowT(paramTy: Type, retTy: Type)
  // record types
  case RecordT(fields: Map[String, Type])
  // bottom type
  case BotT
  // top type
  case TopT

  // the string form of a type
  def str: String = this match
    case NumT                 => "Number"
    case ArrowT(p: ArrowT, b) => s"(${p.str}) => ${b.str}"
    case ArrowT(p, b)         => s"${p.str} => ${b.str}"
    case RecordT(fields)      =>
      s"{${fields.map { case (f, t) => s"$f: ${t.str}" }.mkString(", ")}}"
    case BotT                 => "Bot"
    case TopT                 => "Top"

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

  private val keywords = Set("val", "exit", "Number", "Bot", "Top")
  private val d: String = "0-9"
  private val w: String = "_a-zA-Z"
  private lazy val num: P[BigInt] = s"-?[$d]+".r ^^ BigInt.apply
  private lazy val id: P[String] = s"[$w][$w$d]*".r.withFilter(!keywords(_))
  lazy val expr: P[Expr] =
    import PostfixOps.*
    lazy val e0: P[Expr] =
      "val" ~> id ~ opt(":" ~> ty) ~ ("=" ~> e1) ~ (opt(";") ~> e0) ^^ {
        case x ~ t ~ e ~ b => Val(x, t, e, b)
      } | e1
    lazy val e1: P[Expr] = rep1sep(e2, "+") ^^ { _.reduceLeft(Add.apply) }
    lazy val e2: P[Expr] = rep1sep(e3, "*") ^^ { _.reduceLeft(Mul.apply) }
    lazy val e3: P[Expr] = e4 ~ rep(
      "(" ~> e0 <~ ")" ^^ PApp.apply |
      "." ~> id ^^ PAccess.apply
    ) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (f, PApp(a)) => App(f, a)
        case (e, PAccess(f)) => Access(e, f)
      }
    }

    lazy val e4: P[Expr] = (
      ("(" ~> id <~ ":") ~ ty ~ (")" ~ "=>" ~> e0) ^^ {
        case p ~ t ~ b => Fun(p, t, b)
      } |
      "{" ~> repsep(id ~ ("=" ~> e0), ",") <~ "}" ^^ {
        case fs =>
          dupCheck(fs.map { case f ~ t => f }, "field name")
          Record(fs.map { case f ~ e => f -> e })
      } |
      "(" ~> e0 <~ ")" |
      "{" ~> e0 <~ "}" |
      "exit" ^^^ Exit |
      num ^^ Num.apply |
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
      "Bot" ^^^ BotT |
      "Top" ^^^ TopT |
      "{" ~> repsep(id ~ (":" ~> t0), ",") <~ "}" ^^ {
        case fs =>
          dupCheck(fs.map { case f ~ t => f }, "field name")
          RecordT(fs.map { case f ~ t => f -> t }.toMap)
      }
    )
    t0

  // postfix operators
  private enum PostfixOps:
    case PApp(arg: Expr)
    case PAccess(field: String)

  // duplicate check
  private def dupCheck[T](
    names: List[T],
    kind: String,
    f: T => String = (x: T) => x.toString,
  ): List[T] =
    if (names.distinct.length != names.length)
      error(s"duplicate $kind: ${names.mkString(", ")}")
    names
}
