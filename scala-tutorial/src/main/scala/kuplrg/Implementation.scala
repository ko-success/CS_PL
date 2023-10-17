package kuplrg

object Implementation extends Template {

  def sqsum(x: Int, y: Int): Int = x * x + y * y

  def concat(left: String, right: String): String = left + right

  def subN(n: Int): Int => Int = (x: Int) => x - n

  def twice(f: Int => Int): Int => Int = (x: Int) => f(f(x))

  def compose(f: Int => Int, g: Int => Int): Int => Int = (x: Int) => f(g(x))

  def sumOnlyOdd(l: List[Int]): Int = l.filter(_ % 2 != 0).sum

  def foldWith(f: (Int, Int) => Int): List[Int] => Int = (l: List[Int]) => l.foldLeft(0)(f)

  def fromIndex(l: List[Int], from: Int, set: Set[Int]) : Set[Int] = l match
    case Nil => set
    case h :: t => 
      if(from <= 0) {fromIndex(t, from - 1, set + h)}
      else  {fromIndex(t, from - 1, set)}

  def toSet(l: List[Int], from: Int): Set[Int] = fromIndex(l, from, Set())

  def getOrZero(map: Map[String, Int], key: String): Int = map.getOrElse(key, 0)

  def setMinus(s1: Set[Int], s2: Set[Int]): Set[Int] = s1.diff(s2)

  // ---------------------------------------------------------------------------
  // Trees
  // ---------------------------------------------------------------------------
  import Tree.*

  def has(value: Int): Tree => Boolean = (t: Tree) => t match
    case Leaf(v) => if (v == value) then true else false
    case Branch(l, v, r) => (v == value) || has(value)(l) || has(value)(r)

  // -------------------------------------------------------------
  def optionMax(x: Option[Int], y: Option[Int]) = (x, y) match
    case (Some(n), Some(m)) => if n > m then n else m
    case (Some(n), None) => Some(n)
    case (None, Some(m)) => Some(m)
    case (None, None) => None

  def intMax(x: Int, y: Int) = if x > y then x else y

  def maxDepth(t: Tree, value: Int, d: Int): Option[Int] = t match
    case Leaf(v) if v == value => Some(d)
    case Branch(l, v, r) => 
      val ld = maxDepth(l, value, d + 1)
      val rd = maxDepth(r, value, d + 1)
      (ld, rd) match
        case (Some(ld_), Some(rd_)) => Some(intMax(ld_, rd_))
        case (Some(ld_), None) => Some(ld_)
        case (None, Some(rd_)) => Some(rd_)
        case (None, None) => None
    case _ => None
  // --------------------------------------------------------------

  def maxDepthOf(value: Int): Tree => Option[Int] = (t: Tree) => maxDepth(t, value, 0)

  def mul(t: Tree): Int = t match
    case Leaf(v) => v
    case Branch(l, v, r) => mul(l) * v * mul(r)

  def countLeaves(t: Tree): Int = t match
    case Leaf(_) => 1
    case Branch(l, _, r) => countLeaves(l) + countLeaves(r)

  def postOrder(t: Tree): List[Int] = t match
    case Leaf(v) => List(v)
    case Branch(l, v, r) => 
      postOrder(l) ++ postOrder(r) ++ List(v)

  // ---------------------------------------------------------------------------
  // Boolean Expressions
  // ---------------------------------------------------------------------------
  import BE.*

  def countLiterals(expr: BE): Int = expr match 
    case True => 1
    case False => 1
    case And(l, r) => countLiterals(l) + countLiterals(r)
    case Or(l, r) => countLiterals(l) + countLiterals(r)
    case Not(n) => countLiterals(n)
  
  def countNots(expr: BE): Int = expr match
    case True => 0
    case False => 0
    case And(l, r) => countNots(l) + countNots(r)
    case Or(l, r) => countNots(l) + countNots(r)
    case Not(n) => 1 + countNots(n)

  def depth(expr: BE): Int = expr match
    case True => 0
    case False => 0
    case And(l, r) => 1 + math.max(depth(l), depth(r))
    case Or(l, r) => 1 + math.max(depth(l), depth(r))
    case Not(n) => 1 + depth(n)

  def getString(expr: BE): String = expr match
    case True => "true"
    case False => "false"
    case And(l, r) => s"(${getString(l)} & ${getString(r)})"
    case Or(l, r) => s"(${getString(l)} | ${getString(r)})"
    case Not(n) => s"!${getString(n)}"

  def eval(expr: BE): Boolean = expr match
    case True => true
    case False => false
    case And(l, r) => eval(r) && eval(r)
    case Or(l, r) => eval(l) || eval(r)
    case Not(n) => !eval(n)
}
