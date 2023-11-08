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
  // interp (continuation with first-class functions)
  // -------------------------------------------------------------------------
  test(evalCPS("1 + 2 * 3 + 4"), "11")
  test(evalCPS("x => x + 1"), "<function>")
  test(evalCPS("(x => (y => y * 2)(x + 1))(42)"), "86")
  test(evalCPS("(x => (y => (x => x * 2)(y + 1))(x + 1))(6)"), "16")
  test(evalCPS("(f => x => y => x + y)(1)"), "<function>")
  test(evalCPS(expr1), "23")
  test(evalCPS(expr2), "106")
  testExc(evalCPS(expr3), "free identifier")
  testExc(evalCPS(expr4), "free identifier")
  test(evalCPS(expr5), "21")
  testExc(evalCPS(expr6), "free identifier")
  testExc(evalCPS(expr7), "free identifier")
  testExc(evalCPS(expr8), "invalid operation")
  testExc(evalCPS(expr9), "free identifier")
  testExc(evalCPS(expr10), "free identifier")
  test(evalCPS(expr11), "4")
  test(evalCPS(expr12), "9")
  test(evalCPS(expr13), "16")
  test(evalCPS(expr14), "53")
  test(evalCPS(expr15), "268")
  test(evalCPS(expr16), "268")
  testExc(evalCPS(expr17), "not a function")
  test(evalCPS(expr18), "16")
  test(evalCPS(expr19), "8")
  test(evalCPS(expr20), "8")

  // -------------------------------------------------------------------------
  // interp (first-order representation of continuations)
  // -------------------------------------------------------------------------
  test(evalK("1 + 2 * 3 + 4"), "11")
  test(evalK("x => x + 1"), "<function>")
  test(evalK("(x => (y => y * 2)(x + 1))(42)"), "86")
  test(evalK("(x => (y => (x => x * 2)(y + 1))(x + 1))(6)"), "16")
  test(evalK("(f => x => y => x + y)(1)"), "<function>")
  test(evalK(expr1), "23")
  test(evalK(expr2), "106")
  testExc(evalK(expr3), "free identifier")
  testExc(evalK(expr4), "free identifier")
  test(evalK(expr5), "21")
  testExc(evalK(expr6), "free identifier")
  testExc(evalK(expr7), "free identifier")
  testExc(evalK(expr8), "invalid operation")
  testExc(evalK(expr9), "free identifier")
  testExc(evalK(expr10), "free identifier")
  test(evalK(expr11), "4")
  test(evalK(expr12), "9")
  test(evalK(expr13), "16")
  test(evalK(expr14), "53")
  test(evalK(expr15), "268")
  test(evalK(expr16), "268")
  testExc(evalK(expr17), "not a function")
  test(evalK(expr18), "16")
  test(evalK(expr19), "8")
  test(evalK(expr20), "8")

  // -------------------------------------------------------------------------
  // deep expressions
  // -------------------------------------------------------------------------
  lazy val deepExpr1 = (1 to 10000).mkString("+")   // 1+2+3+...+10000
  lazy val deepExpr2 = "1" + ("+1" * 49999)         // 1+1+1+...+1
  lazy val deepExpr3 = "1" + ("+2+(-1)" * 49999)    // 1+2+(-1)+...+2+(-1)
  test(evalK(deepExpr1), "50005000", weight = 2)
  test(evalK(deepExpr2), "50000", weight = 4)
  test(evalK(deepExpr3), "50000", weight = 4)

  /* Write your own tests */
}
