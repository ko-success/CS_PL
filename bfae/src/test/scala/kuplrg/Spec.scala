package kuplrg

import Implementation.*

class Spec extends SpecBase {

  // -------------------------------------------------------------------------
  // interp
  // -------------------------------------------------------------------------
  test(eval("1 + 2"), "3")
  test(eval("3 * 5"), "15")
  testExc(eval("1 + (x => x + 1)"), "invalid operation")
  testExc(eval("(x => x + 1) * 5"), "invalid operation")
  testExc(eval("x"), "free identifier")
  test(eval("x => x"), "<function>")
  test(eval("(x => x + 1)(2)"), "3")
  testExc(eval("1(2)"), "not a function")
  test(eval("val x = 2; x + 1"), "3")
  test(eval("val x = 2; val y = 3; x + y"), "5")
  test(eval("val x = 2; val x = 3; x + 1"), "4")
  test(eval("val addN = n => m => n + m; val add3 = addN(3); add3(2)"), "5")
  test(eval("Box(42)"), "<box>")
  test(eval("val x = Box(42); x.get"), "42")
  testExc(eval("1.get"), "not a box")
  test(eval("val x = Box(42); x.set(3); x.get"), "3")
  test(eval("val x = Box(42); x.set(x.get + 1); x.get"), "43")
  test(eval("val x = Box(42); x.set(3) + x.get"), "6")
  test(eval("val x = Box(42); x.set(3) * x.get"), "9")
  testExc(eval("1.set(2)"), "not a box")
  val expr1 = """
    val inc = x => x.set(x.get + 1);
    val b = Box(7);
    inc(b);
    inc(b);
    b.get
  """
  test(eval(expr1), "9")
  val expr2 = """
    val b = Box(1);
    val f = x => x + b.get;
    f(3) * { b.set(2); f(3) }
  """
  test(eval(expr2), "20")
  val expr3 = """
    val x = Box(3);
    val y = x;
    x.set(7);
    y.get
  """
  test(eval(expr3), "7")
  val expr4 = """
    val x = Box(3);
    val y = Box(x);
    val z = Box(x);
    y.get.set(7);
    x.get
  """
  test(eval(expr4), "7")
  val expr5 = """
    val b = Box(1);
    val f = x => x + b.get;
    val x1 = f(3);
    b.set(2);
    val x2 = f(3);
    b.set(3);
    val x3 = f(3);
    x1 * x2 * x3
  """
  test(eval(expr5), "120")

  /* Write your own tests */
}
