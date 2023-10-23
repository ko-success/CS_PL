package kuplrg

import Implementation.*

class Spec extends SpecBase {

  // -------------------------------------------------------------------------
  // interp
  // -------------------------------------------------------------------------
  test(eval("1 + 2"), "3")
  test(eval("5 * 3"), "15")
  test(eval("1 + { val x = 2; x }"), "3")
  test(eval("{ val x = 1; x } + 2"), "3")
  test(eval("3 * { val x = 2; x }"), "6")
  test(eval("{ val x = 5; x } * 5"), "25")
  testExc(eval("1 + (x => x)"), "invalid operation")
  testExc(eval("(x =>x ) + 2"), "invalid operation")
  testExc(eval("1 + { val f = x => x; f }"), "invalid operation")
  testExc(eval("{ val f = x => x; f } + 2"), "invalid operation")
  testExc(eval("x"), "free identifier")
  testExc(eval("x + 2"), "free identifier")
  test(eval("x => x"), "<function>")
  test(eval("val x = 1 + 2; x"), "<expr>")
  test(eval("val x = 1 + 2; x + 3"), "6")
  test(eval("val x = 1; val y = 2; x + y"), "3")
  test(eval("val f = x => x * 2; val x = 3; f(x)"), "6")
  test(eval("val x = 1 + (x => x); 42"), "42")
  test(eval("val f = x => 42; f(1 + (x => x)) * 2"), "84")
  testExc(eval("42(1)"), "not a function")

  /* Write your own tests */

  // -------------------------------------------------------------------------
  // interp Call by Need
  // -------------------------------------------------------------------------
  test(evalN("1 + 2"), "3")
  test(evalN("5 * 3"), "15")
  test(evalN("1 + { val x = 2; x }"), "3")
  test(evalN("{ val x = 1; x } + 2"), "3")
  test(evalN("3 * { val x = 2; x }"), "6")
  test(evalN("{ val x = 5; x } * 5"), "25")
  testExc(evalN("1 + (x => x)"), "invalid operation")
  testExc(evalN("(x =>x ) + 2"), "invalid operation")
  testExc(evalN("1 + { val f = x => x; f }"), "invalid operation")
  testExc(evalN("{ val f = x => x; f } + 2"), "invalid operation")
  testExc(evalN("x"), "free identifier")
  testExc(evalN("x + 2"), "free identifier")
  test(evalN("x => x"), "<function>")
  test(evalN("val x = 1 + 2; x"), "<expr>")
  test(evalN("val x = 1 + 2; x + 3"), "6")
  test(evalN("val x = 1; val y = 2; x + y"), "3")
  test(evalN("val f = x => x * 2; val x = 3; f(x)"), "6")
  test(evalN("val x = 1 + (x => x); 42"), "42")
  test(evalN("val f = x => 42; f(1 + (x => x)) * 2"), "84")
  testExc(evalN("42(1)"), "not a function")
}
