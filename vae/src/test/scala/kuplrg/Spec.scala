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
  test(eval("val x = 42; x"), "42")
  test(eval("val x = 42; x + 1"), "43")
  test(eval("val x = 42; val y = x + 1; y * 2"), "86")
  test(eval("val x = 42; val x = x + 1; x * 2"), "86")
  test(eval("val x = 6; val y = x + 1; val x = y + 1; x * 2"), "16")
  test(eval("""
    val x = 2;
    val x = x * 2;
    val y = x + 1;
    val x = y + 1;
    x * 2
  """), "12")
  test(eval("""
    val x = 2;
    val y = x + 1;
    val z = y * 3;
    x + y + z
  """), "14")
  test(eval("""
    val x = 2;
    val y = 5;
    { val x = 3; x * y } + x
  """), "17")
  test(eval("""
    val x = 2;
    { val y = x + 1; y + 2 } * { val y = x * 2; x + y }
  """), "30")
  test(eval("""{
    val x = 3;
    val y = x + 1;
    val z = x * y;
    x + y + z
  } * {
    val x = 2;
    { val y = x + 1; y + 2 } * { val y = x * 2; x + y }
  }"""), "570")
  testExc(eval("x"), "free identifier")
  testExc(eval("val x = 1; y"), "free identifier")
  testExc(eval("val x = x; x"), "free identifier")
  testExc(eval("{ val x = 1; 1 } * { val y = 2; x }"), "free identifier")
  testExc(eval("{ val x = 1; 1 } + x"), "free identifier")

  // -------------------------------------------------------------------------
  // freeIds
  // -------------------------------------------------------------------------
  test(freeIds(Expr("val x = 3; x + 3 * x")), Set())
  test(freeIds(Expr("val x = 3; a * 4 + x")), Set("a"))
  test(freeIds(Expr("val x = 3; b * a * x")), Set("a", "b"))
  test(freeIds(Expr("val x = 3; a * b * (x + b)")), Set("a", "b"))
  test(freeIds(Expr("val x = 3; y * val y = 7; x + b * a")), Set("a", "b", "y"))
  test(freeIds(Expr("val x = t; x * val y = y; x + b * a")), Set("a", "b", "t", "y"))
  test(freeIds(Expr("val x = val y = 3; x * y; x + y")), Set("x", "y"))
  test(freeIds(Expr("{ val x = 10; val x = 3; y * { val y = 7; x + c * b } } + { val a = a; a }")), Set("a", "b", "c", "y"))
  test(freeIds(Expr("{ val x = 10; val x = 3; y * { val y = 7; x + c * b } } + { val a = d; a }")), Set("b", "c", "d", "y"))
  test(freeIds(Expr("{ val x = 10; val x = 3; y * { val y = 7; x + c * b } } + { val a = d; z }")), Set("b", "c", "d", "y", "z"))

  // -------------------------------------------------------------------------
  // bindingIds
  // -------------------------------------------------------------------------
  test(bindingIds(Expr("3 + x * y")), Set())
  test(bindingIds(Expr("val x = 3; x + 3 * x")), Set("x"))
  test(bindingIds(Expr("val x = 3; val y = x; x + y")), Set("x", "y"))
  test(bindingIds(Expr("val x = 3; val y = x; x + 3 * y")), Set("x", "y"))
  test(bindingIds(Expr("val x = 3; val y = x; val z = y; x + 3 * z")), Set("x", "y", "z"))
  test(bindingIds(Expr("val y = 3; val x = x; y")), Set("x", "y"))
  test(bindingIds(Expr("val y = 3; val y = x; x + y")), Set("y"))
  test(bindingIds(Expr("val y = 3; val y = { val x = 3 + y; x * y }; x + y")), Set("x", "y"))
  test(bindingIds(Expr("val z = 3; val w = { val z = 3 + y; x * y }; val w = y; (7 + w)")), Set("w", "z"))
  test(bindingIds(Expr("val z = 3; val w = { val z = 3 + y; x + y }; val w = y; (7 + w)")), Set("w", "z"))

  // -------------------------------------------------------------------------
  // boundIds
  // -------------------------------------------------------------------------
  test(boundIds(Expr("val x = 3; y + 3")), Set())
  test(boundIds(Expr("val x = 3; x + x * y")), Set("x"))
  test(boundIds(Expr("val x = 3; x + { val y = 7; x * y }")), Set("x", "y"))
  test(boundIds(Expr("val x = 3; val y = x; 3 * y")), Set("x", "y"))
  test(boundIds(Expr("val x = 3; y + { val y = x; 3 * 7 }")), Set("x"))
  test(boundIds(Expr("val x = x; y + { val y = y; 3 * { val z = 7; (z * x) } }")), Set("x", "z"))
  test(boundIds(Expr("val x = { val y = 3; (x + y) }; y + { val y = y; 3 * 7 }")), Set("y"))
  test(boundIds(Expr("val x = a; val y = b; val z = c; d + x * (y + z)")), Set("x", "y", "z"))
  test(boundIds(Expr("{ val x = 10; val x = 3; y * { val y = 7; x + c * b } } + { val a = d; a }")), Set("a", "x"))
  test(boundIds(Expr("{ val x = 10; val x = 3; y * { val y = 7; x + c * b } } + { val a = d; z }")), Set("x"))

  // -------------------------------------------------------------------------
  // shadowedIds
  // -------------------------------------------------------------------------
  test(shadowedIds(Expr("val x = 3; x + x * y")), Set())
  test(shadowedIds(Expr("val x = 3; val x = x + 1; x + x * y")), Set("x"))
  test(shadowedIds(Expr("val x = 3; val y = x + 1; x + y * y")), Set())
  test(shadowedIds(Expr("val x = 3; val y = x; 3 * y")), Set())
  test(shadowedIds(Expr("val x = 3; y + { val y = x; 3 * 7 }")), Set())
  test(shadowedIds(Expr("val x = x; y + { val y = y; 3 * { val z = 7; z * x } }")), Set())
  test(shadowedIds(Expr("val x = a; val y = b; val z = c; d + x * (y + z)")), Set())
  test(shadowedIds(Expr("{ val x = 42; val x = x + 1; x * 2 } + { val y = 3; val y = x * 3; y }")), Set("x", "y"))
  test(shadowedIds(Expr("{ val x = 10; val x = 3; y * { val y = 7; x + c * b } } + { val a = d; a }")), Set("x"))
  test(shadowedIds(Expr("{ val x = 8; val x = 3; y * { val y = 7; x + c * b } } + { val a = d; z }")), Set("x"))

  /* Write your own tests */
}
