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
  case Fun(params: List[Param], body: Expr)
  // recursive functions
  case Rec(x: String, params: List[Param], rty: Type, body: Expr, scope: Expr)
  // function applications
  case App(fun: Expr, args: List[Expr])
  // conditional
  case If(cond: Expr, thenExpr: Expr, elseExpr: Expr)
  // algebraic data type
  case TypeDef(name: String, varts: List[Variant], body: Expr)
  // pattern matching
  case Match(expr: Expr, mcases: List[MatchCase])

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
    case Fun(ps, b)   => s"{ (${ps.map(_.str).mkString(", ")}) => ${b.str} }"
    case Rec(n, ps, rt, b, s) =>
      val psStr = ps.map(_.str).mkString(", ")
      s"{ def $n($psStr): ${rt.str} = ${b.str}; ${s.str} }"
    case App(f, es)   => s"${f.str}(${es.map(_.str).mkString(", ")})"
    case If(c, t, e)  => s"if (${c.str}) ${t.str} else ${e.str}"
    case TypeDef(x, vs, b) =>
      s"{ enum $x { ${vs.map(_.str).mkString("; ")} }; ${b.str} }"
    case Match(e, cs) =>
      s"{ ${e.str} match { ${cs.map(_.str).mkString("; ")} } }"

// parameter definition
case class Param(name: String, ty: Type):
  // the string form of a parameter
  def str: String = s"$name: ${ty.str}"

// variant definition
case class Variant(name: String, ptys: List[Type]):
  // the string form of a variant
  def str: String = s"case $name(${ptys.map(_.str).mkString(", ")})"

// match case
case class MatchCase(name: String, params: List[String], body: Expr):
  // the string form of a case
  def str: String = s"case $name(${params.mkString(", ")}) => ${body.str}"

// environments
type Env = Map[String, Value]

// values
enum Value:
  // number values
  case NumV(number: BigInt)
  // boolean values
  case BoolV(bool: Boolean)
  // closure values
  case CloV(params: List[String], body: Expr, env: () => Env)
  // constructor values
  case ConstrV(name: String)
  // variant values
  case VariantV(name: String, values: List[Value])

  // the string form of a value
  def str: String = this match
    case NumV(n)         => n.toString
    case BoolV(b)        => b.toString
    case CloV(ps, b, e)  => "<function>"
    case ConstrV(n)      => s"<constructor: $n>"
    case VariantV(n, vs) => s"$n(${vs.map(_.str).mkString(", ")})"

// type environments
case class TypeEnv(
  vars: Map[String, Type] = Map(),
  tys: Map[String, Map[String, List[Type]]] = Map(),
) {
  def addVar(pair: (String, Type)): TypeEnv =
    TypeEnv(vars + pair, tys)
  def addVars(pairs: Iterable[(String, Type)]): TypeEnv =
    TypeEnv(vars ++ pairs, tys)
  def addType(tname: String, ws: Map[String, List[Type]]): TypeEnv =
    TypeEnv(vars, tys + (tname -> ws))
}

// types
enum Type:
  // number types
  case NumT
  // boolean types
  case BoolT
  // function (arrow) types
  case ArrowT(paramTys: List[Type], retTy: Type)
  // type names
  case NameT(name: String)

  // the string form of a type
  def str: String = this match
    case NumT                       => "Number"
    case BoolT                      => "Boolean"
    case NameT(x)                   => x
    case ArrowT(List(p: ArrowT), b) => s"(${p.str}) => ${b.str}"
    case ArrowT(List(p), b)         => s"${p.str} => ${b.str}"
    case ArrowT(ps, b) =>
      s"(${ps.map(_.str).mkString(", ")}) => ${b.str}"

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
    "true", "false", "def", "if", "else", "val", "enum", "case", "match",
    "Number", "Boolean",
  )
  private val d: String = "0-9"
  private val w: String = "_a-zA-Z"
  private lazy val num: P[BigInt] = s"-?[$d]+".r ^^ BigInt.apply
  private lazy val bool: P[Boolean] = "true" ^^^ true | "false" ^^^ false
  private lazy val id: P[String] = s"[$w][$w$d]*".r.withFilter(!keywords(_))

  // expressions
  lazy val expr: P[Expr] =
    lazy val e0: P[Expr] =
      ("enum" ~> id) ~
      ("{" ~> rep1(variant <~ opt(";")) <~ "}" <~ opt(";")) ~
      e0 ^^ {
        case x ~ vs ~ b => TypeDef(x, dupCheck(vs, "variant", _.name), b)
      } |
      ("val" ~> id) ~ ("=" ~> e0 <~ opt(";")) ~ (e0) ^^ {
        case x ~ i ~ b => Val(x, i, b)
      } |
      "def" ~> id ~ params ~ (":" ~> ty) ~ ("=" ~> e0 <~ opt(";")) ~ e0 ^^ {
        case x ~ ps ~ rt ~ b ~ s => Rec(x, ps, rt, b, s)
      } |
      "if" ~> ("(" ~> e0 <~ ")") ~ e0 ~ ("else" ~> e0) ^^ {
        case c ~ t ~ e => If(c, t, e)
      } |
      params ~ ("=>" ~> e0) ^^ {
        case ps ~ b => Fun(ps, b)
      } |
      e1 ~ rep("match" ~ "{" ~> rep(mcase <~ opt(";")) <~ "}") ^^ {
        case e ~ cs => cs.foldLeft(e)(Match.apply)
      }
    lazy val e1: P[Expr] = rep1sep(e2, "||") ^^ (_.reduceLeft(Or))
    lazy val e2: P[Expr] = rep1sep(e3, "&&") ^^ (_.reduceLeft(And))
    lazy val e3: P[Expr] = e4 ~ rep(("==" | "!=") ~ e4) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (l, "==" ~ r) => Eq(l, r)
        case (l, _ ~ r) => Ne(l, r)
      }
    }
    lazy val e4: P[Expr] = e5 ~ rep(("<=" | "<" | ">=" | ">") ~ e5) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (l, "<" ~ r)  => Lt(l, r)
        case (l, "<=" ~ r) => Le(l, r)
        case (l, ">" ~ r)  => Gt(l, r)
        case (l, _ ~ r) => Ge(l, r)
      }
    }
    lazy val e5: P[Expr] = e6 ~ rep(("+" | "-") ~ e6) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (l, "+" ~ r) => Add(l, r)
        case (l, _ ~ r) => Sub(l, r)
      }
    }
    lazy val e6: P[Expr] = e7 ~ rep(("*" | "/" | "%") ~ e7) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (l, "*" ~ r) => Mul(l, r)
        case (l, "/" ~ r) => Div(l, r)
        case (l, _ ~ r) => Mod(l, r)
      }
    }
    lazy val e7: P[Expr] = "-" ~> e7 ^^ Neg | "!" ~> e7 ^^ Not | e8
    lazy val e8: P[Expr] = e9 ~ rep("(" ~> repsep(e0, ",") <~")") ^^ {
      case f ~ as => as.foldLeft(f)(App.apply)
    }
    lazy val e9: P[Expr] = (
      "(" ~> e0 <~ ")" |
      "{" ~> e0 <~ "}" |
      num ^^ Num.apply |
      bool ^^ Bool.apply |
      id ^^ Id.apply
    )
    e0

  // variants
  private lazy val variant: P[Variant] =
    "case" ~> id ~ ("(" ~> repsep(ty, ",") <~ ")") ^^ {
      case x ~ tys => Variant(x, tys)
    }

  // match cases
  private lazy val mcase: P[MatchCase] =
    "case" ~> id ~ ("(" ~> repsep(id, ",") <~ ")") ~ ("=>" ~> expr) ^^ {
      case x ~ ps ~ b => MatchCase(x, dupCheck(ps, "parameter"), b)
    }

  // parameters
  private lazy val params: P[List[Param]] =
    "(" ~> repsep(param, ",") <~ ")" ^^ { case ps => dupCheck(ps, "parameter") }
  private lazy val param: P[Param] =
    id ~ (":" ~> ty) ^^ { case x ~ t => Param(x, t) }

  // types
  private lazy val ty: P[Type] =
    lazy val t0: P[Type] =
      ("(" ~> repsep(t0, ",") <~ ")") ~ ("=>" ~> t0) ^^ {
        case ps ~ b => ArrowT(ps, b)
      } |
      t1 ~ ("=>" ~> t0) ^^ {
        case p ~ b => ArrowT(List(p), b)
      } |
      t1
    lazy val t1: P[Type] = (
      "(" ~> t0 <~ ")" |
      "Number" ^^^ NumT |
      "Boolean" ^^^ BoolT |
      id ^^ NameT.apply
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

  // duplicate check
  private def dupCheck(ss: List[String]): Boolean =
    ss.distinct.length != ss.length

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
