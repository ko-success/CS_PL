package kuplrg

import Implementation.*

class Spec extends SpecBase {

  // -------------------------------------------------------------------------
  // eval
  // -------------------------------------------------------------------------
  test(eval("42"), "42: Number")
  test(eval("1 + 2"), "3: Number")
  test(eval("2 * 3"), "6: Number")
  test(eval("5 / 2"), "2: Number")
  test(eval("5 % 3"), "2: Number")
  test(eval("1 + 2 * 5 / 3 % 2"), "2: Number")
  test(eval("true"), "true: Boolean")
  test(eval("false"), "false: Boolean")
  test(eval("1 == 2"), "false: Boolean")
  test(eval("1 < 2"), "true: Boolean")
  test(eval("1 <= 2"), "true: Boolean")
  test(eval("1 > 2"), "false: Boolean")
  test(eval("1 >= 2"), "false: Boolean")
  test(eval("false && true"), "false: Boolean")
  test(eval("false || true"), "true: Boolean")
  // invalid operation: 42 / 0
  testExc(eval("42 / 0"))
  // invalid operation: 42 % 0
  testExc(eval("42 % (4 - 2 * 2)"))
  // type mismatch: Boolean != Number
  testExc(eval("1 + true"))
  // type mismatch: Number != Boolean
  testExc(eval("1 && true"))
  // type mismatch: Number != Boolean
  testExc(eval("1 || true"))
  // free identifier: x
  testExc(eval("x"))
  test(eval("val x = 42; x"), "42: Number")
  test(eval("val x = 1; val y = 2; x + y"), "3: Number")
  test(eval("(x: Number) => x"), "<function>: Number => Number")
  test(eval("(x: Boolean) => x"), "<function>: Boolean => Boolean")
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
  test(eval(
    "(f: Number => Boolean) => (x: Number) => f(x)"
  ), "<function>: (Number => Boolean) => Number => Boolean")
  test(eval(
    "(x: Number) => (y: Number) => x * y"
  ),"<function>: Number => Number => Number")
  test(eval(
    "def f(x: Number): Number = x + 1; f"
  ), "<function>: Number => Number")
  test(eval(
    "def f(x: Number): Number = x + 1; f(10)"
  ), "11: Number")
  test(eval(
    "def sum(n: Number): Number = if (n > 0) n + sum(n - 1) else 0; sum"
  ), "<function>: Number => Number")
  test(eval(
    "def sum(n: Number): Number = if (n > 0) n + sum(n - 1) else 0; sum(10)"
  ), "55: Number")
  test(eval(
    "def fib(k: Number): Number = if (k < 2) 1 else fib(k-1) + fib(k-2); fib"
  ), "<function>: Number => Number")
  test(eval(
    "def fib(k: Number): Number = if (k < 2) 1 else fib(k-1) + fib(k-2); fib(7)"
  ), "21: Number")
  val expr1 = """
    def gcd(a: Number): Number => Number = (b: Number) => {
      if (a == 0) b else gcd(b % a)(a)
    };
    gcd(432)(180)
  """
  test(eval(expr1), "36: Number")
  val expr2 = """
    def fact(n: Number): Number = if (n > 0) n * fact(n - 1) else 1;
    fact(10)
  """
  test(eval(expr2), "3628800: Number")
  val expr3 = """
    def isPrime(n: Number): Boolean =
      def iter(k: Number): Boolean =
        if (k * k > n) true
        else if (n % k == 0) false
        else iter(k + 1);
      if (n < 2) false
      else iter(2);
    isPrime(1000003)
  """
  test(eval(expr3), "true: Boolean")
  val expr4 = """
    def sum(n: Number): Number =
      def iter(k: Number): Number => Number = (acc: Number) =>
        if (k > n) acc
        else iter(k + 1)(acc + k);
      iter(1)(0);
    sum(100)
  """
  test(eval(expr4), "5050: Number")
  val expr5 = """
    def findKth(f: Number => Boolean): Number => Number = (k: Number) =>
      def iter(n: Number): Number => Number = (k: Number) =>
        if (f(n)) {
          if (k == 1) n else iter(n + 1)(k - 1)
        } else {
          iter(n + 1)(k)
        };
      iter(0)(k);
    findKth((x: Number) => x % 3 == 2)(43)
  """
  test(eval(expr5), "128: Number")
  val expr6 = """
    def isPrime(n: Number): Boolean =
      def iter(k: Number): Boolean =
        if (k * k > n) true
        else if (n % k == 0) false
        else iter(k + 1);
      if (n < 2) false
      else iter(2);
    def findKth(f: Number => Boolean): Number => Number = (k: Number) =>
      def iter(n: Number): Number => Number = (k: Number) =>
        if (f(n)) {
          if (k == 1) n else iter(n + 1)(k - 1)
        } else {
          iter(n + 1)(k)
        };
      iter(0)(k);
    findKth(isPrime)(101)
  """
  test(eval(expr6), "547: Number")
  val expr7 = """
    val addN = (n: Number) => (x: Number) => x + n;
    def isEven(x: Number): Boolean = x % 2 == 0;
    def findKth(f: Number => Boolean): Number => Number = (k: Number) =>
      def iter(n: Number): Number => Number = (k: Number) =>
        if (f(n)) {
          if (k == 1) n else iter(n + 1)(k - 1)
        } else {
          iter(n + 1)(k)
        };
      iter(0)(k);
    findKth((n: Number) => isEven(addN(n)(1)))(23)
  """
  test(eval(expr7), "45: Number")
  val expr8 = """
    val sum = (n: Number) => {
      (map: Number => Number) => {
        (filter: Number => Boolean) => {
          def iter(k: Number): Number => Number = (acc: Number) =>
            if (k > n) acc
            else iter(k + 1)(acc + (if (filter(k)) map(k) else 0));
          iter(1)(0)
        }
      }
    };
    val square = (x: Number) => x * x;
    val isOdd = (x: Number) => x % 2 == 1;
    sum(10)(square)(isOdd)
  """
  test(eval(expr8), "165: Number")
  val expr9 = """
    val fold = {
      (f: Number => Number => Number) =>
      (default: Number) =>
      (from: Number) =>
      (to: Number) =>
        def iter(k: Number): Number => Number = (acc: Number) =>
          if (k > to) acc
          else iter(k + 1)(f(acc)(k));
        iter(from)(default)
    };
    val add = (x: Number) => (y: Number) => x + y;
    fold(add)(0)(53)(78)
  """
  test(eval(expr9), "1703: Number")
  val expr10 = """
    val fold = {
      (f: Number => Number => Number) =>
      (default: Number) =>
      (from: Number) =>
      (to: Number) =>
      (map: Number => Number) =>
      (filter: Number => Boolean) =>
        def iter(k: Number): Number => Number = (acc: Number) =>
          if (k > to) acc
          else iter(k + 1)(f(acc)(if (filter(k)) map(k) else 0));
        iter(from)(default)
    };
    val add = (x: Number) => (y: Number) => x + y;
    val square = (x: Number) => x * x;
    val isOdd = (x: Number) => x % 2 == 1;
    fold(add)(0)(7)(33)(square)(isOdd)
  """
  test(eval(expr10), "6510: Number")

  /* Write your own tests */

}
