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

  // the string form of an expression
  def str: String = this match
    case Num(n)    => n.toString
    case Add(l, r) => s"(${l.str} + ${r.str})"
    case Mul(l, r) => s"(${l.str} * ${r.str})"

// values
type Value = BigInt

// -----------------------------------------------------------------------------
// Parsers
// -----------------------------------------------------------------------------
object Expr extends Parser.From(Parser.expr)
object Parser extends RegexParsers with PackratParsers {
  type P[+T] = PackratParser[T]
  class From[T](p: Parser[T]) {
    def apply(s: String): T = parseAll(p, s).getOrElse(error("parse error"))
  }
  private lazy val num: P[BigInt] = "-?[0-9]+".r ^^ { BigInt(_) }
  lazy val expr: P[Expr] =
    import Expr.*
    lazy val e0: P[Expr] = (e0 <~ "+") ~ e1 ^^ { case l ~ r => Add(l, r) } | e1
    lazy val e1: P[Expr] = (e1 <~ "*") ~ e2 ^^ { case l ~ r => Mul(l, r) } | e2
    lazy val e2: P[Expr] = num              ^^ { case n     => Num(n)    } | e3
    lazy val e3: P[Expr] = "(" ~> e0 <~ ")"
    e0
}
