package kuplrg

import Implementation.*

class Spec extends SpecBase {

  val expr1 = """
    var inc = x => x += 1;
    var b = 7;
    inc(b);
    inc(b);
    b
  """

  val expr2 = """
    var b = 1;
    var f = x => x + b;
    f(3) * { b = 2; f(3) }
  """

  val expr3 = """
    var x = 3;
    var y = x;
    x = 7;
    y
  """

  val expr4 = """
    var x = 3;
    x += x *= x += 2;
    x
  """

  val expr5 = """
    var addN = x => y => x + y;
    var f = addN(3);
    var twice = x => { f(x); f(x) };
    twice(1) + {
      f = x => x += 1;
      var y = 3;
      twice(y);
      y
    }
  """

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
  test(eval("var x = 2; x + 1"), "3")
  test(eval("var x = 2; var y = 3; x + y"), "5")
  test(eval("var x = 2; var x = 3; x + 1"), "4")
  test(eval("var addN = n => m => n + m; var add3 = addN(3); add3(2)"), "5")
  test(eval("var x = 2; x += 1; x"), "3")
  test(eval("var x = 3; x *= 5; x"), "15")
  test(eval("var x = 1; var f = y => x + y; var x = 2; x = 3; f(4)"), "5")
  test(eval(expr1), "7")
  test(eval(expr2), "20")
  test(eval(expr3), "3")
  test(eval(expr4), "18")
  test(eval(expr5), "7")

  // -------------------------------------------------------------------------
  // interpCBR
  // -------------------------------------------------------------------------
  test(evalCBR("1 + 2"), "3")
  test(evalCBR("3 * 5"), "15")
  testExc(evalCBR("1 + (x => x + 1)"), "invalid operation")
  testExc(evalCBR("(x => x + 1) * 5"), "invalid operation")
  testExc(evalCBR("x"), "free identifier")
  test(evalCBR("x => x"), "<function>")
  test(evalCBR("(x => x + 1)(2)"), "3")
  testExc(evalCBR("1(2)"), "not a function")
  test(evalCBR("var x = 2; x + 1"), "3")
  test(evalCBR("var x = 2; var y = 3; x + y"), "5")
  test(evalCBR("var x = 2; var x = 3; x + 1"), "4")
  test(evalCBR("var addN = n => m => n + m; var add3 = addN(3); add3(2)"), "5")
  test(evalCBR("var x = 2; x += 1; x"), "3")
  test(evalCBR("var x = 3; x *= 5; x"), "15")
  test(evalCBR("var x = 1; var f = y => x + y; var x = 2; x = 3; f(4)"), "5")
  test(evalCBR(expr1), "9")
  test(evalCBR(expr2), "20")
  test(evalCBR(expr3), "3")
  test(evalCBR(expr4), "18")
  test(evalCBR(expr5), "9")

  /* Write your own tests */
}
