package kuplrg

import Implementation.*

class Spec extends SpecBase {

  // -------------------------------------------------------------------------
  // eval
  // -------------------------------------------------------------------------
  test(eval("42"), "42")
  test(eval("1 + 2"), "3")
  test(eval("1 + 2 * 3"), "7")
  test(eval("(1 + 2) * 3"), "9")
  test(eval("1 + 2 * 3 + 4"), "11")

  // -------------------------------------------------------------------------
  // countNums
  // -------------------------------------------------------------------------
  test(countNums(Expr("42")), 1)
  test(countNums(Expr("1 + 2")), 2)
  test(countNums(Expr("1 + 2 * 3")), 3)
  test(countNums(Expr("(1 + 2) * 3")), 3)
  test(countNums(Expr("1 + 2 * 3 + 4")), 4)

  /* Write your own tests */
}
