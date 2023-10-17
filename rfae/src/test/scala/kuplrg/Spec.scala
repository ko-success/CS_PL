package kuplrg

import Implementation.*

class Spec extends SpecBase {

  // -------------------------------------------------------------------------
  // interp
  // -------------------------------------------------------------------------
  test(eval("1 + -(2 * 3)"), "-5")
  test(eval("15 / 2 - 35 / 7 + -(2 * 3) % 36"), "-4")
  testExc(eval("42 / 0"), "invalid operation")
  testExc(eval("42 % (4 - 2 * 2)"), "invalid operation")
  testExc(eval("3 % true"), "invalid operation")
  testExc(eval("1 + true"), "invalid operation")
  testExc(eval("false * 3"), "invalid operation")
  testExc(eval("false / true"), "invalid operation")
  test(eval("(1 + 2 == 3) && (4 < 5)"), "true")
  test(eval("(0 - 10 > -3) && (true + false)"), "false")
  test(eval("(8 / 2 <= 3) || (3 >= 5)"), "false")
  test(eval("(44 / 2 != 21) || (false + 1)"), "true")
  test(eval("true && (false || true)"), "true")
  test(eval("true && (false || true) && (false || false)"), "false")
  test(eval("! (true && (false || true))"), "false")
  testExc(eval("true == true"), "invalid operation")
  testExc(eval("42 != false"), "invalid operation")
  testExc(eval("42 < false"), "invalid operation")
  testExc(eval("x"), "free identifier")
  test(eval("x => x + 1"), "<function>")
  test(eval("(x => x * 3)(4)"), "12")
  test(eval("def f(x) = x + 1; f"), "<function>")
  test(eval("def f(x) = x + 1; f(10)"), "11")
  test(eval("def sum(n) = if (n > 0) n + sum(n - 1) else 0; sum(10)"), "55")
  test(eval("def fib(k) = if (k < 2) 1 else fib(k-1) + fib(k-2); fib(7)"), "21")
  testExc(eval("42(10)"), "not a function")
  test(eval("if (1 + 2 == 3) 10 else 20"), "10")
  test(eval("if (5 < 3) 10 else 20"), "20")
  testExc(eval("if (42) 10 else 20"), "not a boolean")
  test(eval("if (2 >= 3) true + false else 10"), "10")
  val expr1 = """
    def gcd(a) = b => if (a == 0) b else gcd(b % a)(a);
    gcd(432)(180)
  """
  test(eval(expr1), "36")
  val expr2 = """
    def fact(n) = if (n > 0) n * fact(n - 1) else 1;
    fact(10)
  """
  test(eval(expr2), "3628800")
  val expr3 = """
    def isPrime(n) =
      def iter(k) =
        if (k * k > n) true
        else if (n % k == 0) false
        else iter(k + 1);
      if (n < 2) false
      else iter(2);
    isPrime(1000003)
  """
  test(eval(expr3), "true")
  val expr4 = """
    def sum(n) =
      def iter(k) = acc =>
        if (k > n) acc
        else iter(k + 1)(acc + k);
      iter(1)(0);
    sum(100)
  """
  test(eval(expr4), "5050")
  val expr5 = """
    def findKth(f) = k =>
      def iter(n) = k =>
        if (f(n)) {
          if (k == 1) n else iter(n + 1)(k - 1)
        } else {
          iter(n + 1)(k)
        };
      iter(0)(k);
    findKth(x => x % 3 == 2)(43)
  """
  test(eval(expr5), "128")
  val expr6 = """
    def isPrime(n) =
      def iter(k) =
        if (k * k > n) true
        else if (n % k == 0) false
        else iter(k + 1);
      if (n < 2) false
      else iter(2);
    def findKth(f) = k =>
      def iter(n) = k =>
        if (f(n)) {
          if (k == 1) n else iter(n + 1)(k - 1)
        } else {
          iter(n + 1)(k)
        };
      iter(0)(k);
    findKth(isPrime)(101)
  """
  test(eval(expr6), "547")
  val expr7 = """
    def addN(n) = x => x + n;
    def isEven(x) = x % 2 == 0;
    def findKth(f) = k =>
      def iter(n) = k =>
        if (f(n)) {
          if (k == 1) n else iter(n + 1)(k - 1)
        } else {
          iter(n + 1)(k)
        };
      iter(0)(k);
    findKth(n => isEven(addN(n)(1)))(23)
  """
  test(eval(expr7), "45")
  val expr8 = """
    def sum(n) = map => filter =>
      def iter(k) = acc =>
        if (k > n) acc
        else iter(k + 1)(acc + (if (filter(k)) map(k) else 0));
      iter(1)(0);
    sum(10)(x => x * x)(x => x % 2 == 1)
  """
  test(eval(expr8), "165")
  val expr9 = """
    def fold(f) = default => from => to =>
      def iter(k) = acc =>
        if (k > to) acc
        else iter(k + 1)(f(acc)(k));
      iter(from)(default);
    fold(x => y => x + y)(0)(53)(78)
  """
  test(eval(expr9), "1703")
  val expr10 = """
    def fold(f) = default => from => to => map => filter =>
      def iter(k) = acc =>
        if (k > to) acc
        else iter(k + 1)(f(acc)(if (filter(k)) map(k) else 0));
      iter(from)(default);
    fold(x => y => x + y)(0)(7)(33)(x => x * x)(x => x % 2 == 1)
  """
  test(eval(expr10), "6510")

  /* Write your own tests */
}
