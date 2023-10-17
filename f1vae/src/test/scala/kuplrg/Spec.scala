package kuplrg

import Implementation.*

class Spec extends SpecBase {

  val program1 = """
    def square(x) = x * x;
    square(3) + square(4)
  """
  val program2 = """
    def double(x) = x * 2;
    def add3(y) = y + 3;
    def mul5(z) = z * 5;
    double(add3(mul5(10)))
  """
  val program3 = """
    def f(x) = x + y;
    f(10)
  """
  val program4 = """
    def f(x) = x + 1;
    g(10)
  """
  val program5 = """
    def double(x) = x * 2;
    def triple(y) = double(y) + y;
    def mul5(z) = double(z) + triple(z);
    mul5(10)
  """
  val program6 = """
    def f(x) = x + y;
    val y = 1;
    f(2)
  """
  val program7 = """
    def f(x) = x + y;
    { val y = 1; f(2) } + { val y = 10; f(20) }
  """
  val program8 = """
    def f(x) = x + y;
    { val y = 1; f(2) } + f(10)
  """
  val program9 = """
    def f(z) = z * x + y;
    def g(y) = f(20);
    def h(x) = g(10);
    h(2)
  """
  val program10 = """
    def f(z) = z * x + y;
    def g(y) = f(20);
    def h(x) = g(10);
    h(2) + { val x = 3; g(5) * { val y = 4; f(6) } }
  """

  // -------------------------------------------------------------------------
  // interp (static scoping)
  // -------------------------------------------------------------------------
  test(eval(program1), "25")
  test(eval(program2), "106")
  testExc(eval(program3), "free identifier")
  testExc(eval(program4), "unknown function")
  test(eval(program5), "50")
  testExc(eval(program6), "free identifier")
  testExc(eval(program7), "free identifier")
  testExc(eval(program8), "free identifier")
  testExc(eval(program9), "free identifier")
  testExc(eval(program10), "free identifier")

  // -------------------------------------------------------------------------
  // interpDS (dynamic scoping)
  // -------------------------------------------------------------------------
  test(evalDS(program1), "25")
  test(evalDS(program2), "106")
  testExc(evalDS(program3), "free identifier")
  testExc(evalDS(program4), "unknown function")
  test(evalDS(program5), "50")
  test(evalDS(program6), "3")
  test(evalDS(program7), "33")
  testExc(evalDS(program8), "free identifier")
  test(evalDS(program9), "50")
  test(evalDS(program10), "1480")

  /* Write your own tests */
}
