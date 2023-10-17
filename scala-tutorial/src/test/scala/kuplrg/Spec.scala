package kuplrg

import Implementation.*

class Spec extends SpecBase {
  // tests for `sqsum`
  test(sqsum(0, 0), 0)
  test(sqsum(2, 3), 13)
  test(sqsum(-3, 4), 25)

  // tests for `concat`
  test(concat("Hello ", "World!"), "Hello World!")
  test(concat("COSE", "212"), "COSE212")
  test(concat("COSE", "215"), "COSE215")

  // tests for `subN`
  test(subN(3)(5), 2)
  test(subN(4)(13), 9)
  test(subN(243)(-942), -1185)

  // tests for `twice`
  test(twice(_ + 3)(1), 7)
  test(twice(subN(3))(10), 4)
  test(twice(_ * 10)(42), 4200)

  // tests for `compose`
  test(compose(_ + 3, _ * 2)(1), 5)
  test(compose(_ * 10, _ + 1)(42), 430)
  test(compose(subN(3), subN(2))(10), 5)

  // tests for `sumOnlyOdd`
  test(sumOnlyOdd(List(2)), 0)
  test(sumOnlyOdd(List(1, 2, 3)), 4)
  test(sumOnlyOdd(List(4, 2, 3, 7, 5)), 15)

  // tests for `foldWith`
  test(foldWith(_ + _)(List(1, 2, 3)), 6)
  test(foldWith(_ - _)(List(5, 9, 2, 3)), -19)
  test(foldWith(_ * 2 + _)(List(4, 7, 3, 2)), 68)

  // tests for `toSet`
  test(toSet(List(1, 5, 2, 7, 4, 2, 4), 0), Set(1, 2, 4, 5, 7))
  test(toSet(List(1, 5, 2, 7, 4, 2, 4), 2), Set(2, 4, 7))
  test(toSet(List(1, 5, 2, 7, 4, 2, 4), 4), Set(2, 4))

  // tests for `getOrZero`
  val m: Map[String, Int] = Map("Park" -> 3, "Kim" -> 5)
  test(getOrZero(m, "Park"), 3)
  test(getOrZero(m, "Lee"), 0)
  test(getOrZero(m, "Kim"), 5)

  // tests for `setMinus`
  test(setMinus(Set(1, 2, 3), Set(2, 3, 4)), Set(1))
  test(setMinus(Set(1, 2, 3), Set(4, 5, 6)), Set(1, 2, 3))
  test(setMinus(Set(1, 2, 3), Set(1, 2, 3, 4)), Set())

  // ---------------------------------------------------------------------------
  // Trees
  // ---------------------------------------------------------------------------
  import Tree.*

  //  8
  val tree1: Tree = Leaf(8)

  //    4
  //   / \
  //  5   2
  //     / \
  //    8   3
  val tree2: Tree = Branch(Leaf(5), 4, Branch(Leaf(8), 2, Leaf(3)))

  //    7
  //   / \
  //  2   3
  //     / \
  //    5   1
  //   / \
  //  1   8
  val tree3: Tree = Branch(Leaf(2), 7, Branch(Branch(Leaf(1), 5, Leaf(8)), 3, Leaf(1)))

  // tests for `has`
  test(has(8)(tree1), true)
  test(has(7)(tree2), false)
  test(has(1)(tree3), true)

  // tests for `maxDepthOf`
  test(maxDepthOf(8)(tree1), Some(0))
  test(maxDepthOf(7)(tree2), None)
  test(maxDepthOf(1)(tree3), Some(3))

  // tests for `mul`
  test(mul(tree1), 8)
  test(mul(tree2), 960)
  test(mul(tree3), 1680)

  // tests for `countLeaves`
  test(countLeaves(tree1), 1)
  test(countLeaves(tree2), 3)
  test(countLeaves(tree3), 4)

  // tests for `postOrder`
  test(postOrder(tree1), List(8))
  test(postOrder(tree2), List(5, 8, 3, 2, 4))
  test(postOrder(tree3), List(2, 1, 8, 5, 1, 3, 7))

  // ---------------------------------------------------------------------------
  // Boolean Expressions
  // ---------------------------------------------------------------------------
  import BE.*

  // (true | false)
  val be1: BE = Or(True, False)

  // (!(true | false) & !(false | true))
  val be2: BE = And(Not(Or(True, False)), Not(Or(False, True)))

  // (!((false | !true) & false) & (true & !false))
  val be3: BE = And(Not(And(Or(False, Not(True)), False)), And(True, Not(False)))

  // tests for `countLiterals`
  test(countLiterals(be1), 2)
  test(countLiterals(be2), 4)
  test(countLiterals(be3), 5)

  // tests for `countNots`
  test(countNots(be1), 0)
  test(countNots(be2), 2)
  test(countNots(be3), 3)

  // tests for `depth`
  test(depth(be1), 1)
  test(depth(be2), 3)
  test(depth(be3), 5)

  // tests for `eval`
  test(eval(be1), true)
  test(eval(be2), false)
  test(eval(be3), true)

  // tests for `getString`
  test(getString(be1), "(true | false)")
  test(getString(be2), "(!(true | false) & !(false | true))")
  test(getString(be3), "(!((false | !true) & false) & (true & !false))")

  /* Write your own tests */
}
