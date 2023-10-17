package kuplrg

import scala.util.parsing.combinator.*

// expressions
enum Expr:
  // unit
  case EUnit
  // numbers
  case ENum(number: BigInt)
  // booleans
  case EBool(bool: Boolean)
  // identifier lookups
  case EId(name: String)
  // addition
  case EAdd(left: Expr, right: Expr)
  // multiplication
  case EMul(left: Expr, right: Expr)
  // division
  case EDiv(left: Expr, right: Expr)
  // modulo
  case EMod(left: Expr, right: Expr)
  // equal-to
  case EEq(left: Expr, right: Expr)
  // less-than
  case ELt(left: Expr, right: Expr)
  // conditional
  case EIf(cond: Expr, thenExpr: Expr, elseExpr: Expr)
  // empty list
  case ENil
  // list cons
  case ECons(head: Expr, tail: Expr)
  // list head
  case EHead(list: Expr)
  // list tail
  case ETail(list: Expr)
  // list length
  case ELength(list: Expr)
  // list map function
  case EMap(list: Expr, fun: Expr)
  // list flatMap function
  case EFlatMap(list: Expr, fun: Expr)
  // list filter function
  case EFilter(list: Expr, fun: Expr)
  // tuple
  case ETuple(exprs: List[Expr])
  // tuple projection
  case EProj(tuple: Expr, index: Int)
  // variable definition
  case EVal(name: String, value: Expr, scope: Expr)
  // lambda function
  case EFun(params: List[String], body: Expr)
  // mutually recursive function
  case ERec(defs: List[FunDef], scope: Expr)
  // function application
  case EApp(fun: Expr, args: List[Expr])

  // the string form of an expression
  def str: String = listStr.fold(rawStr)(s => s"List($s)")

  // the raw string form of an expression
  def rawStr: String = this match
    case EUnit           => "()"
    case ENum(n)         => n.toString
    case EBool(b)        => b.toString
    case EId(x)          => x
    case EAdd(l, r)      => s"(${l.str} + ${r.str})"
    case EMul(l, r)      => s"(${l.str} * ${r.str})"
    case EDiv(l, r)      => s"(${l.str} / ${r.str})"
    case EMod(l, r)      => s"(${l.str} % ${r.str})"
    case EEq(l, r)       => s"(${l.str} == ${r.str})"
    case ELt(l, r)       => s"(${l.str} < ${r.str})"
    case EIf(c, t, e)    => s"if (${c.str}) ${t.str} else ${e.str}"
    case ENil            => "Nil"
    case ECons(h, t)     => s"(${h.str} :: ${t.str})"
    case EHead(l)        => s"${l.str}.head"
    case ETail(l)        => s"${l.str}.tail"
    case ELength(l)      => s"${l.str}.length"
    case EMap(l, f)      => s"${l.str}.map(${f.str})"
    case EFlatMap(l, f)  => s"${l.str}.flatMap(${f.str})"
    case EFilter(l, f)   => s"${l.str}.filter(${f.str})"
    case ETuple(es)      => s"(${es.map(_.str).mkString(", ")})"
    case EProj(t, i)     => s"${t.str}._${i + 1}"
    case EVal(x, v, s)   => s"{ val $x = ${v.str}; ${s.str} }"
    case EFun(ps, b)     => s"(${ps.mkString(", ")}) => ${b.str}"
    case ERec(ds, s)     => s"{ ${ds.map(_.str + " ").mkString}${s.str} }"
    case EApp(f, as)     => s"${f.str}(${as.map(_.str).mkString(", ")})"

  // the list string form of an expression
  private def listStr: Option[String] = this match
    case ENil           => Some("")
    case ECons(h, ENil) => Some(h.str)
    case ECons(h, t)    => t.listStr.map(s"${h.str}, " + _)
    case _              => None

// named functions
case class FunDef(name: String, params: List[String], body: Expr) {
  // the string form of a named function
  def str: String = s"def $name(${params.mkString(", ")}) = ${body.str};"
}

// environments
type Env = Map[String, Value]

// values
enum Value:
  // unit value
  case UnitV
  // number value
  case NumV(number: BigInt)
  // boolean value
  case BoolV(bool: Boolean)
  // empty list value
  case NilV
  // list cons value
  case ConsV(head: Value, tail: Value)
  // tuple value
  case TupleV(values: List[Value])
  // closure value
  case CloV(params: List[String], body: Expr, env: () => Env)

  // the string form of a value
  def str: String = listStr.fold(rawStr)(s => s"List($s)")

  // the raw string form of a value
  def rawStr: String = this match
    case UnitV          => "()"
    case NumV(n)        => n.toString
    case BoolV(b)       => b.toString
    case NilV           => "Nil"
    case ConsV(h, t)    => s"(${h.str} :: ${t.str})"
    case TupleV(vs)     => s"(${vs.map(_.str).mkString(", ")})"
    case CloV(p, b, e)  => "<function>"

  // the list string form of a value
  private def listStr: Option[String] = this match
    case NilV           => Some("")
    case ConsV(h, NilV) => Some(h.str)
    case ConsV(h, t)    => t.listStr.map(s"${h.str}, " + _)
    case _              => None

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
  private val keywords = Set(
    "List", "Nil", "def", "else", "false", "for", "if", "true", "val", "yield",
  )
  private val d: String = "0-9"
  private val w: String = "_a-zA-Z"
  private lazy val idx: P[Int] = "_[1-9][0-9]*".r ^^ (_.tail.toInt)
  private lazy val num: P[BigInt] = s"-?[$d]+".r ^^ BigInt.apply
  private lazy val bool: P[Boolean] = "true" ^^^ true | "false" ^^^ false
  private lazy val id: P[String] = s"[$w][$w$d]*".r.withFilter(!keywords(_))
  lazy val expr: P[Expr] =
    import PostfixOps.*
    lazy val e0: P[Expr] = rep1sep(e1, "||") ^^ (_.reduceLeft(EOr))
    lazy val e1: P[Expr] = rep1sep(e2, "&&") ^^ (_.reduceLeft(EAnd))
    lazy val e2: P[Expr] = e3 ~ rep(("==" | "!=") ~ e3) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (l, "==" ~ r) => EEq(l, r)
        case (l, _ ~ r) => ENe(l, r)
      }
    }
    lazy val e3: P[Expr] = e4 ~ rep(("<=" | "<" | ">=" | ">") ~ e4) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (l, "<" ~ r)  => ELt(l, r)
        case (l, "<=" ~ r) => ELe(l, r)
        case (l, ">" ~ r)  => EGt(l, r)
        case (l, _ ~ r) => EGe(l, r)
      }
    }
    lazy val e4: P[Expr] = rep1sep(e5, "::") ^^ (_.reduceRight(ECons.apply))
    lazy val e5: P[Expr] = e6 ~ rep(("+" | "-") ~ e6) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (l, "+" ~ r) => EAdd(l, r)
        case (l, _ ~ r) => ESub(l, r)
      }
    }
    lazy val e6: P[Expr] = e7 ~ rep(("*" | "/" | "%") ~ e7) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (l, "*" ~ r) => EMul(l, r)
        case (l, "/" ~ r) => EDiv(l, r)
        case (l, _ ~ r) => EMod(l, r)
      }
    }
    lazy val e7: P[Expr] = "-" ~> e7 ^^ ENeg | "!" ~> e7 ^^ ENot | e8
    lazy val e8: P[Expr] = e9 ~ rep(
      "(" ~> repsep(e0, ",") <~ ")" ^^ PApp.apply |
      "." ~> "isEmpty" ^^^ PIsEmpty |
      "." ~> "length" ^^^ PLength |
      "." ~> "head" ^^^ PHead |
      "." ~> "tail" ^^^ PTail |
      "." ~> "map" ~> "(" ~> e0 <~ ")" ^^ PMap.apply |
      "." ~> "flatMap" ~> "(" ~> e0 <~ ")" ^^ PFlatMap.apply |
      "." ~> "filter" ~> "(" ~> e0 <~ ")" ^^ PFilter.apply |
      "." ~> idx ^^ PProj.apply
    ) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (f, PApp(as)) => EApp(f, as)
        case (l, PIsEmpty) => EIsEmpty(l)
        case (l, PLength)  => ELength(l)
        case (l, PHead)    => EHead(l)
        case (l, PTail)    => ETail(l)
        case (l, PMap(f))  => EMap(l, f)
        case (l, PFlatMap(f)) => EFlatMap(l, f)
        case (l, PFilter(f)) => EFilter(l, f)
        case (l, PProj(i)) => EProj(l, i)
      }
    }
    lazy val e9: P[Expr] = (
      (id <~ "=>") ~ e0 ^^ { case p ~ b => EFun(List(p), b) } |
      params ~ ("=>" ~> e0) ^^ {
        case ps ~ b => EFun(ps, b)
      } |
      "()" ^^^ EUnit |
      "(" ~> e0 <~ ")" |
      "(" ~> e0 ~ rep1(", " ~> e0) <~ ")" ^^ {
        case e ~ es => ETuple(e :: es)
      } |
      "{" ~> e0 <~ "}" |
      num ^^ ENum.apply |
      bool ^^ EBool.apply |
      rep1(namedFun) ~ e0 ^^ {
        case ds ~ s => ERec(dupCheck(ds, "function", _.name), s)
      } |
      "Nil" ^^^ ENil |
      "List" ~> ("(" ~> repsep(e0, ",") <~ ")") ^^ EList |
      "for" ~> ("{" ~> rep1(listCompElem) <~ "}") ~ ("yield"~> e0) ^^ {
        case ps ~ e => EFor(ps.dropRight(1), ps.last, e)
      } |
      "if" ~> ("(" ~> e0 <~ ")") ~ e0 ~ ("else" ~> e0) ^^ {
        case c ~ t ~ e => EIf(c, t, e)
      } |
      ("val" ~> id <~ "=") ~ e0 ~ (";" ~> e0) ^^ {
        case x ~ i ~ b => EVal(x, i, b)
      } |
      id ^^ EId.apply
    )
    e0

  // parameters
  private lazy val params: P[List[String]] =
    "(" ~> repsep(id, ",") <~ ")" ^^ { dupCheck(_, "parameter") }

  // named functions
  private lazy val namedFun: P[FunDef] =
    "def" ~> id ~ params ~ ("=" ~> expr <~ ";") ^^ {
      case n ~ ps ~ b => FunDef(n, ps, b)
    }

  // postfix operators
  private enum PostfixOps:
    case PApp(args: List[Expr])
    case PIsEmpty
    case PLength
    case PHead
    case PTail
    case PMap(fun: Expr)
    case PFlatMap(fun: Expr)
    case PFilter(fun: Expr)
    case PProj(idx: Int)

  // list comprehension elements
  private case class ListCompElem(name: String, list: Expr, conds: List[Expr]) {
    def base: Expr = conds
      .map(EFun(List(name), _))
      .foldRight(list) { case (f, l) => EFilter(l, f) }
  }
  private lazy val listCompElem: P[ListCompElem] =
    (id <~ "<-") ~ (expr <~ ";") ~ rep("if" ~> expr <~ ";") ^^ {
      case n ~ l ~ cs => ListCompElem(n, l, cs)
    }

  // desugaring rules
  private val T: Expr = EBool(true)
  private val F: Expr = EBool(false)
  private def ENeg(expr: Expr): Expr = EMul(expr, ENum(-1))
  private def ESub(left: Expr, right: Expr): Expr = EAdd(left, ENeg(right))
  private def EAnd(left: Expr, right: Expr): Expr = EIf(left, right, F)
  private def EOr(left: Expr, right: Expr): Expr = EIf(left, T, right)
  private def ENot(expr: Expr): Expr = EIf(expr, F, T)
  private def ENe(left: Expr, right: Expr): Expr = ENot(EEq(left, right))
  private def ELe(left: Expr, right: Expr): Expr =
    EOr(ELt(left, right), EEq(left, right))
  private def EGt(left: Expr, right: Expr): Expr = ENot(ELe(left, right))
  private def EGe(left: Expr, right: Expr): Expr = ENot(ELt(left, right))
  private def EList(exprs: List[Expr]): Expr =
    exprs.foldRight(ENil: Expr)(ECons.apply)
  private def EIsEmpty(expr: Expr): Expr = EEq(expr, ENil)
  private def EFor(
    prevs: List[ListCompElem],
    last: ListCompElem,
    expr: Expr
  ): Expr = prevs.foldRight(EMap(last.base, EFun(List(last.name), expr))) {
    case (prev, acc) => EFlatMap(prev.base, EFun(List(prev.name), acc))
  }

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
