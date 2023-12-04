package kuplrg

import Implementation.*

class Spec extends SpecBase {

  // -------------------------------------------------------------------------
  // eval
  // -------------------------------------------------------------------------
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
  test(eval("forall[T] (x: T) => x"), "<type-abs>: [T](T => T)")
  test(
    eval("val f = forall[T] (x: T) => x; f[Number]"),
    "<function>: Number => Number"
  )
  test(
    eval("val f = forall[T] (x: T) => x; f[Number => Number]"),
    "<function>: (Number => Number) => Number => Number"
  )
  test(
    eval("val f = forall[T] (x: T) => x; val fN = f[Number]; fN(42)"),
    "42: Number"
  )
  test(
    eval("val f = forall[T] (x: T) => x; f[Number => Number](f[Number])"),
    "<function>: Number => Number"
  )
  test(
    eval("val f = forall[T] (x: T) => x; f[Number => Number](f[Number])(42)"),
    "42: Number"
  )
  test(
    eval("((f: [T](T => T)) => f[Number](7))"),
    "<function>: ([T](T => T)) => Number"
  )
  test(
    eval("((f: [T](T => T)) => f[Number](7))(forall[U] (x: U) => x)"),
    "7: Number"
  )
  // not a polymorphic type: Number
  testExc(eval("42[Number]"))
  // unknown type name: T
  testExc(eval("((x: T) => x)(42)"))

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
    val f = forall[T] {
      forall[U] {
        (g: T => U) => (x: T) => g(x)
      }
    }
    f
  """
  test(
    eval(expr6),
    "<type-abs>: [T][U]((T => U) => T => U)",
    weight = 4,
  )

  val expr7 = """
    val f = forall[T] {
      forall[U] {
        (g: T => U) => (x: T) => g(x)
      }
    }
    f[Number]
  """
  test(
    eval(expr7),
    "<type-abs>: [U]((Number => U) => Number => U)",
    weight = 4,
  )

  val expr8 = """
    val f = forall[T] {
      forall[U] {
        (g: T => U) => (x: T) => g(x)
      }
    }
    f[Number][Number => Number]
  """
  test(
    eval(expr8),
    "<function>: (Number => Number => Number) => Number => Number => Number",
    weight = 4,
  )

  val expr9 = """
    val f = forall[T] {
      forall[U] {
        (g: T => U) => (x: T) => g(x)
      }
    }
    f[Number][Number => Number]((x: Number) => (y: Number) => x + y)
  """
  test(
    eval(expr9),
    "<function>: Number => Number => Number",
    weight = 4,
  )

  val expr10 = """
    val f = forall[U] (x: U) => {
      val y = forall[U] x
      y[Number => Number]
    }
    f[Number](1)
  """
  testExc(eval(expr10), weight = 4)

  /* Write your own tests */

}
