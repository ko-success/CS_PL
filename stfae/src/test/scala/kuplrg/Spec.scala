package kuplrg

import Implementation.*

class Spec extends SpecBase {

  // ---------------------------------------------------------------------------
  // eval
  // ---------------------------------------------------------------------------
  test(eval("42"), "42: Number")
  test(eval("1 + 2"), "3: Number")
  test(eval("2 * 3"), "6: Number")
  test(eval("1 + 2 * 3"), "7: Number")
  // free identifier: x
  testExc(eval("x"))
  test(eval("val x = 42; x"), "42: Number")
  test(eval("val x = 1; val y = 2; x + y"), "3: Number")
  test(eval("(x: Number) => x"), "<function>: Number => Number")
  test(eval("val f = (x: Number) => x; f"), "<function>: Number => Number")
  // not a function: Number
  testExc(eval("1(2)"))
  // not a function: Number
  testExc(eval("(x: Number) => x(1)"))
  // type mismatch: Number != Number => Number
  testExc(eval("(x: (Number => Number) => Number) => x(1)"))
  test(eval("val f = (n: Number) => (m: Number) => m+n; f(3)(4)"), "7: Number")
  test(eval("val f = (x: Number) => x; f(42)"), "42: Number")
  test(eval("val f = (x: Number) => (y: Number) => x*y; f(5)(7)"), "35: Number")
  test(eval("{}"), "{}: {}")
  test(eval("{a=1}"), "{a=1}: {a: Number}")
  test(eval("{a={b=2}}"), "{a={b=2}}: {a: {b: Number}}")
  test(eval("{a=1, b=2}"), "{a=1, b=2}: {a: Number, b: Number}")
  test(eval("{a=1, b=2}.a"), "1: Number")
  test(eval("{a=1, b=2}.b"), "2: Number")
  test(eval("(x: {a: Number}) => x.a"), "<function>: {a: Number} => Number")
  test(eval(
    "val x: {a: Number} = {a=1, b=2}; x"
  ), "{a=1, b=2}: {a: Number}")
  test(eval(
    "val x: {b: Number} = {a=1, b=2}; x"
  ), "{a=1, b=2}: {b: Number}")
  test(eval(
    "val x: {a: Number, b: Number} = {b=2, a=1}; x"
  ), "{b=2, a=1}: {a: Number, b: Number}")
  test(eval(
    "val x: {a: Number, b: {}} = {b={c=2}, a=1}; x"
  ), "{b={c=2}, a=1}: {a: Number, b: {}}")
  test(eval(
    "val x: {b: {}} = {b={c=2}, a=1}; x"
  ), "{b={c=2}, a=1}: {b: {}}")
  test(eval(
    "val f: {} => {} = (x: {}) => {}; f"
  ), "<function>: {} => {}")
  test(eval(
    "val f: {a: Number} => {} = (x: {}) => {}; f"
  ), "<function>: {a: Number} => {}")
  test(eval(
    "val f: {} => {} = (x: {}) => {a=1}; f"
  ), "<function>: {} => {}")
  test(eval("exit"), "ERROR: Bot")
  test(eval("val x: Number = exit; x"), "ERROR: Number")
  test(eval("val x: Top = exit; x"), "ERROR: Top")
  test(eval("val x: {a: Bot} = exit; x"), "ERROR: {a: Bot}")
  test(eval("val x: {a: Number} = {a=exit}; x"), "ERROR: {a: Number}")
  test(eval("val x: {a: Top} = {a=2}; x"), "{a=2}: {a: Top}")
  test(eval(
    "val x: {b: Top} = {b={c=2}, a=1}; x"
  ), "{b={c=2}, a=1}: {b: Top}")
  test(eval(
    "val f: {} => Top = (x: {}) => {}; f"
  ), "<function>: {} => Top")
  test(eval(
    "val f: {a: Bot} => {} = (x: {}) => {}; f"
  ), "<function>: {a: Bot} => {}")
  test(eval(
    "val f: Bot => {} = (x: {}) => {a=1}; f"
  ), "<function>: Bot => {}")
  val expr1 = """
    (x: Number) => {
      val y = x + 1;
      (z: Number) => z + y * z
    }
  """
  test(
    eval(expr1),
    "<function>: Number => Number => Number"
  )

  val expr2 = """
    (f: Number => Number) => {
      val x = f(1) + f(2);
      f(f(f(x)))
    }
  """
  test(
    eval(expr2),
    "<function>: (Number => Number) => Number"
  )

  val expr3 = """
    (f: Number => Number => Number) => {
      val g = f(1);
      val h = f(2);
      g(h(f(3)(4)))
    }
  """
  test(
    eval(expr3),
    "<function>: (Number => Number => Number) => Number"
  )

  val expr4 = """
    (f: (Number => Number) => (Number => Number)) => {
      val g = (x: Number) => x + 1;
      val h = f(g);
      f(f(f(h)))(0)
    }
  """
  test(
    eval(expr4),
    "<function>: ((Number => Number) => Number => Number) => Number"
  )

  val expr5 = """
    (f: (Number => Number) => (Number => Number)) => {
      val g = (x: Number) => x + 1;
      val h = f(g);
      f((x: Number) => x + 1)(3) + h(0)
    }
  """
  test(
    eval(expr5),
    "<function>: ((Number => Number) => Number => Number) => Number"
  )
  val expr6 = """
    (x: {a: Number}) => {
      val y = {b = x.a + 1};
      (z: {b: Number}) => z.b + x.a * z.b
    }
  """
  test(
    eval(expr6),
    "<function>: {a: Number} => {b: Number} => Number"
  )

  val expr7 = """
    (f: Number => Number) => {
      val x: Number = f(1) + exit;
      f(f(f(x)))
    }
  """
  test(
    eval(expr7),
    "<function>: (Number => Number) => Number"
  )

  val expr8 = """
    (f: Number => Number => Number) => {
      val g: Number => Number = exit;
      val h = f(2);
      g(h(f(3)(4)))
    }
  """
  test(
    eval(expr8),
    "<function>: (Number => Number => Number) => Number"
  )

  val expr9 = """
    (f: (Number => Number) => (Top => Bot)) => {
      val g = (x: Number) => x + 1;
      val h = f(g);
      f(h)(0)
    }
  """
  test(
    eval(expr9),
    "<function>: ((Number => Number) => Top => Bot) => Bot"
  )

  val expr10 = """
    (f: (Number => Number) => (Number => Number)) => {
      val g: Number => Number = (x: Number) => exit;
      val h: Number => Number = f(g);
      f((x: Number) => x + 1)(3) + h(0)
    }
  """
  test(
    eval(expr10),
    "<function>: ((Number => Number) => Number => Number) => Number"
  )

  /* Write your own tests */

}
