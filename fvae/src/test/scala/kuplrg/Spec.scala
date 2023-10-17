package kuplrg

import Implementation.*

class Spec extends SpecBase {

  val expr1 = """
    val twice = y => y * 2;
    val f = x => twice(x) + 3;
    f(10)
  """
  val expr2 = """
    val twice = y => y * 2;
    val add3 = y => y + 3;
    val mul5 = z => z * 5;
    twice(add3(mul5(10)))
  """
  val expr3 = """
    val f = x => x + y;
    f(10)
  """
  val expr4 = """
    val f = x => x + 1;
    g(10)
  """
  val expr5 = """
    val f = x => x + 1;
    val g = y => y * 2;
    f(g(10))
  """
  val expr6 = """
    val f = x => x + y;
    val y = 1;
    f(2)
  """
  val expr7 = """
    val f = x => x + y;
    { val y = 1; f(2) } + { val y = 10; f(20) }
  """
  val expr8 = """
    val f = x => x + 1;
    val g = h => f * h;
    { val f = 1; g(2) } + { val f = 10; g(20) }
  """
  val expr9 = """
    val f = z => z * x + y;
    val g = y => f(20);
    val h = x => g(10);
    h(2)
  """
  val expr10 = """
    val f = z => z * x + y;
    val g = y => f(20);
    val h = x => g(10);
    h(2) + { val x = 3; g(5) * val y = 4; f(6) }
  """
  val expr11 = """
    val f = g => g(1);
    val add3 = x => x + 3;
    f(add3)
  """
  val expr12 = """
    val f = g => g(1);
    val add3 = x => x + 3;
    val mul5 = x => x * 5;
    f(add3) + f(mul5)
  """
  val expr13 = """
    val twice = f => x => f(f(x));
    val add3 = x => x + 3;
    twice(add3)(10)
  """
  val expr14 = """
    val compose = f => g => x => f(g(x));
    val add3 = x => x + 3;
    val mul5 = x => x * 5;
    compose(add3)(mul5)(10)
  """
  val expr15 = """
    val twice = f => x => f(f(x));
    val compose = f => g => x => f(g(x));
    val add3 = x => x + 3;
    val mul5 = x => x * 5;
    twice(compose(add3)(mul5))(10)
  """
  val expr16 = """
    val twice = f => x => f(f(x));
    val compose = f => g => x => f(g(x));
    val add3 = x => x + 3;
    val mul5 = x => x * 5;
    val add3mul5 = compose(add3)(mul5);
    twice(add3mul5)(10)
  """
  val expr17 = """
    val f = 42;
    val g = x => f(x + 1);
    val f = x => x * 2;
    g(10)
  """
  val expr18 = """
    val x = 10;
    val twice = f => f(f(x));
    val x = 20;
    val add3 = x => x + 3;
    twice(add3)
  """
  val expr19 = """
    val addN = n => x => x + n;
    addN(3)(5)
  """
  val expr20 = """
    val addN = n => x => x + n;
    val n = 10;
    addN(3)(5)
  """

  // -------------------------------------------------------------------------
  // interp (static scoping)
  // -------------------------------------------------------------------------
  test(eval("1 + 2 * 3 + 4"), "11")
  test(eval("x => x + 1"), "<function>")
  test(eval("val x = 42; val y = x + 1; y * 2"), "86")
  test(eval("val x = 6; val y = x + 1; val x = y + 1; x * 2"), "16")
  test(eval("val f = x => y => x + y; f(1)"), "<function>")
  testExc(eval("1 + x => x + 1"), "invalid operation")
  testExc(eval("val f = x => y; f + f"), "invalid operation")
  testExc(eval("1(2)"), "not a function")
  testExc(eval("val f = 1 + 2; f(42)"), "not a function")
  testExc(eval("42(x => x)"), "not a function")
  test(eval(expr1), "23")
  test(eval(expr2), "106")
  testExc(eval(expr3), "free identifier")
  testExc(eval(expr4), "free identifier")
  test(eval(expr5), "21")
  testExc(eval(expr6), "free identifier")
  testExc(eval(expr7), "free identifier")
  testExc(eval(expr8), "invalid operation")
  testExc(eval(expr9), "free identifier")
  testExc(eval(expr10), "free identifier")
  test(eval(expr11), "4")
  test(eval(expr12), "9")
  test(eval(expr13), "16")
  test(eval(expr14), "53")
  test(eval(expr15), "268")
  test(eval(expr16), "268")
  testExc(eval(expr17), "not a function")
  test(eval(expr18), "16")
  test(eval(expr19), "8")
  test(eval(expr20), "8")

  // -------------------------------------------------------------------------
  // interpDS (dynamic scoping)
  // -------------------------------------------------------------------------
  test(evalDS("(1 + 2) * 3"), "9")
  test(evalDS("x => x + 1"), "<function>")
  test(evalDS("val x = 42; val y = x + 1; y * 2"), "86")
  test(evalDS("val x = 6; val y = x + 1; val x = y + 1; x * 2"), "16")
  test(evalDS("val f = x => y => x + y; f(1)"), "<function>")
  testExc(evalDS("1 + x => x + 1"), "invalid operation")
  testExc(evalDS("val f = x => y; f + f"), "invalid operation")
  testExc(evalDS("1(2)"), "not a function")
  testExc(evalDS("val f = 1 + 2; f(42)"), "not a function")
  testExc(evalDS("42(x => x)"), "not a function")
  test(evalDS(expr1), "23")
  test(evalDS(expr2), "106")
  testExc(evalDS(expr3), "free identifier")
  testExc(evalDS(expr4), "free identifier")
  test(evalDS(expr5), "21")
  test(evalDS(expr6), "3")
  test(evalDS(expr7), "33")
  test(evalDS(expr8), "202")
  test(evalDS(expr9), "50")
  test(evalDS(expr10), "1480")
  test(evalDS(expr11), "4")
  test(evalDS(expr12), "9")
  testExc(evalDS(expr13), "free identifier")
  testExc(evalDS(expr14), "free identifier")
  testExc(evalDS(expr15), "free identifier")
  testExc(evalDS(expr16), "free identifier")
  test(evalDS(expr17), "22")
  test(evalDS(expr18), "26")
  testExc(evalDS(expr19), "free identifier")
  test(evalDS(expr20), "15")

  /* Write your own tests */
}
