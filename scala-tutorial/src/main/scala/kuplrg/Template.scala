package kuplrg

trait Template {

  def sqsum(x: Int, y: Int): Int

  def concat(left: String, right: String): String

  def subN(n: Int): Int => Int

  def twice(f: Int => Int): Int => Int

  def compose(f: Int => Int, g: Int => Int): Int => Int

  def sumOnlyOdd(l: List[Int]): Int

  def foldWith(f: (Int, Int) => Int): List[Int] => Int

  def toSet(l: List[Int], from: Int): Set[Int]

  def getOrZero(map: Map[String, Int], key: String): Int

  def setMinus(s1: Set[Int], s2: Set[Int]): Set[Int]

  // ---------------------------------------------------------------------------
  // Trees
  // ---------------------------------------------------------------------------
  enum Tree:
    case Leaf(value: Int)
    case Branch(left: Tree, value: Int, right: Tree)

  def has(value: Int): Tree => Boolean

  def maxDepthOf(value: Int): Tree => Option[Int]

  def mul(t: Tree): Int

  def countLeaves(t: Tree): Int

  def postOrder(t: Tree): List[Int]

  // ---------------------------------------------------------------------------
  // Boolean Expressions
  // ---------------------------------------------------------------------------
  enum BE:
    case True
    case False
    case And(left: BE, right: BE)
    case Or(left: BE, right: BE)
    case Not(expr: BE)

  def countLiterals(expr: BE): Int

  def countNots(expr: BE): Int

  def depth(expr: BE): Int

  def getString(expr: BE): String

  def eval(expr: BE): Boolean
}
