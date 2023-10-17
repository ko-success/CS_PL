package kuplrg

import Implementation.*

class Spec extends SpecBase {

  val expr1 = """
    (twice => {
      (f => {
        f(10)
      })(x => twice(x) + 3)
    })(y => y * 2)
  """
  val expr2 = """
    (twice => {
      (add3 => {
        (mul5 => {
          twice(add3(mul5(10)))
        })(z => z * 5)
      })(y => y + 3)
    })(x => x * 2)
  """
  val expr3 = "(f => f(10))(x => x + y)"
  val expr4 = "(f => g(10))(x => x + 1)"
  val expr5 = """
    (f => {
      (g => {
        f(g(10))
      })(y => y * 2)
    })(x => x + 1)
  """
  val expr6 = """
    (f => {
      (y => {
        f(2)
      })(1)
    })(x => x + y)
  """
  val expr7 = """
    (f => {
      (y => f(2))(1) + (y => f(20))(10)
    })(x => x + y)
  """
  val expr8 = """
    (f => {
      (g => {
        (f => g(2))(1) + (f => g(20))(10)
      })(h => f * h)
    })(x => x + 1)
  """
  val expr9 = """
    (f => {
      (g => {
        (h => {
          h(2)
        })(x => g(10))
      })(y => f(20))
    })(z => z * x + y)
  """
  val expr10 = """
    (f => {
      (g => {
        (h => {
          h(2) + (x => g(5) * (y => f(6))(4))(3)
        })(x => g(10))
      })(y => f(20))
    })(z => z * x + y)
  """
  val expr11 = """
    (f => {
      (add3 => {
        f(add3)
      })(x => x + 3)
    })(g => g(1))
  """
  val expr12 = """
    (f => {
      (add3 => {
        (mul5 => {
          f(add3) + f(mul5)
        })(x => x * 5)
      })(x => x + 3)
    })(g => g(1))
  """
  val expr13 = """
    (twice => {
      (add3 => {
        twice(add3)(10)
      })(x => x + 3)
    })(f => x => f(f(x)))
  """
  val expr14 = """
    (compose => {
      (add3 => {
        (mul5 => {
          compose(add3)(mul5)(10)
        })(x => x * 5)
      })(x => x + 3)
    })(f => g => x => f(g(x)))
  """
  val expr15 = """
    (twice => {
      (compose => {
        (add3 => {
          (mul5 => {
            twice(compose(add3)(mul5))(10)
          })(x => x * 5)
        })(x => x + 3)
      })(f => g => x => f(g(x)))
    })(f => x => f(f(x)))
  """
  val expr16 = """
    (twice => {
      (compose => {
        (add3 => {
          (mul5 => {
            (add3mul5 => {
              twice(add3mul5)(10)
            })(compose(add3)(mul5))
          })(x => x * 5)
        })(x => x + 3)
      })(f => g => x => f(g(x)))
    })(f => x => f(f(x)))
  """
  val expr17 = """
    (f => {
      (g => {
        (f => {
          g(10)
        })(x => x * 2)
      })(x => f(x + 1))
    })(42)
  """
  val expr18 = """
    (x => {
      (twice => {
        (x => {
          (add3 => {
            twice(add3)
          })(x => x + 3)
        })(20)
      })(f => f(f(x)))
    })(10)
  """
  val expr19 = """
    (addN => {
      addN(3)(5)
    })(n => x => x + n)
  """
  val expr20 = """
    (addN => {
      (n => {
        addN(3)(5)
      })(10)
    })(n => x => x + n)
  """

  // -------------------------------------------------------------------------
  // interp (static scoping)
  // -------------------------------------------------------------------------
  test(eval("1 + 2 * 3 + 4"), "11")
  test(eval("x => x + 1"), "<function>")
  test(eval("(x => (y => y * 2)(x + 1))(42)"), "86")
  test(eval("(x => (y => (x => x * 2)(y + 1))(x + 1))(6)"), "16")
  test(eval("(f => x => y => x + y)(1)"), "<function>")
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
  test(evalDS("(x => (y => y * 2)(x + 1))(42)"), "86")
  test(evalDS("(x => (y => (x => x * 2)(y + 1))(x + 1))(6)"), "16")
  test(evalDS("(f => x => y => x + y)(1)"), "<function>")
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
