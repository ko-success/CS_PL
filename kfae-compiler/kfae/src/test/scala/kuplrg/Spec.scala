package kuplrg

import Implementation.*

class Spec extends SpecBase {
  test(eval("1 + 2 * 3 + 4"), "11")
  test(eval("x => x + 1"), "<function>")
  testExc(eval("1 + (x => x)"), "invalid operation")
  test(eval("(x => y => x + y)(1)(2)"), "3")
  testExc(eval("{ val x = 1; x } + x"), "free identifier")
  test(eval("val x = 1; val y = 2; x + y"), "3")
  test(eval("{ vcc k; k }"), "<continuation>")
  testExc(eval("{ vcc k; k + 1 }"), "invalid operation")
  test(eval("{ vcc k; k(1) + 2 }"), "1")
  test(eval("1 + { vcc k; k(2) + 5 }"), "3")
  test(eval("{ vcc k; k(3) + 4 }"), "3")
  test(eval("{ vcc k; 2 + k(y => (y + y)) }(3)"), "6")
  testExc(eval("{ vcc x; x }(5)"), "not a function")
  test(eval("{ vcc x; x }(y => y)(5)"), "5")
  test(eval("val k = { vcc x; x }; k"), "<continuation>")
  val expr1 = """
    val f = 1;
    { vcc k; f(k(42)) }
  """
  test(eval(expr1), "42")
  val expr2 = """
    val f = x => { vcc return; return(x) + 2 };
    f(3) + 1
  """
  test(eval(expr2), "4")
  val expr3 = """
    val f = x => {
      vcc return;
      val y = return(x);
      y + 2
    };
    f(3) + 1
  """
  test(eval(expr3), "4")
  val expr4 = """
    val x = 1;
    val y = { vcc k; 5 * k(2) };
    x + y
  """
  test(eval(expr4), "3")
  val expr5 = """
    {
      vcc done;
      val f = { vcc esc; done(1 + { vcc k; esc(k) }) };
      f(3)
    }
  """
  test(eval(expr5), "4")

  /* Write your own tests */
}
