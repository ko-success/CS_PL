package kuplrg

import Implementation.*

class Spec extends SpecBase {

  // -------------------------------------------------------------------------
  // interp
  // -------------------------------------------------------------------------
  test(eval("()"), "()")
  test(eval("42"), "42")
  test(eval("true"), "true")
  test(eval("false"), "false")
  testExc(eval("x"), "free identifier")
  test(eval("-(-42)"), "42")
  test(eval("1 + 2"), "3")
  test(eval("19 - 2"), "17")
  test(eval("2 * 3"), "6")
  test(eval("15 / 2"), "7")
  testExc(eval("3 / 0"), "invalid operation")
  test(eval("15 % 2"), "1")
  testExc(eval("3 % 0"), "invalid operation")
  test(eval("!true"), "false")
  test(eval("!false"), "true")
  test(eval("true && true"), "true")
  test(eval("true && false"), "false")
  test(eval("false && true"), "false")
  test(eval("false && false"), "false")
  test(eval("true || true"), "true")
  test(eval("true || false"), "true")
  test(eval("false || true"), "true")
  test(eval("false || false"), "false")
  test(eval("1 == 1"), "true")
  test(eval("1 == 2"), "false")
  test(eval("1 != 1"), "false")
  test(eval("true == true"), "true")
  test(eval("true == false"), "false")
  test(eval("Nil == Nil"), "true")
  test(eval("Nil == (1 :: Nil)"), "false")
  test(eval("(2 :: Nil) == Nil"), "false")
  test(eval("(1 :: Nil) == (1 :: Nil)"), "true")
  test(eval("List(1, 2, 3) == 1 :: 2 :: 3 :: Nil"), "true")
  test(eval("(1, 2, 3) == (1, 2, 3)"), "true")
  test(eval("(1, 2, 3) == (1, 2, 3, 4)"), "false")
  test(eval("(1, 2, 3) == (1, 6, 4)"), "false")
  testExc(eval("true == 1"), "invalid operation")
  testExc(eval("(x => x + 1) != 42"), "invalid operation")
  testExc(eval("(x => x + 1) == (x => x + 1)"), "invalid operation")
  testExc(eval("(1, 2, 3) != List(1, 2, 3)"), "invalid operation")
  test(eval("1 < 2"), "true")
  test(eval("2 >= 5"), "false")
  test(eval("1 <= 1"), "true")
  test(eval("1 > 1"), "false")
  testExc(eval("1 < true"), "invalid operation")
  test(eval("if (3 < 5) 10 else 20"), "10")
  test(eval("if (3 > 5) 10 else 20"), "20")
  test(eval("if (3 < 5) 10 else 1 + true"), "10")
  test(eval("if (3 > 5) 1 + true else 20"), "20")
  testExc(eval("if (3) 10 else 20"), "not a boolean")
  test(eval("Nil"), "List()")
  test(eval("1 :: 2 :: 3 :: Nil"), "List(1, 2, 3)")
  test(eval("List()"), "List()")
  test(eval("List(4, 2, 3)"), "List(4, 2, 3)")
  test(eval("List(6, 3, 4).head"), "6")
  testExc(eval("Nil.head"), "empty list")
  testExc(eval("42.head"), "not a list")
  test(eval("List(6, 3, 4).tail"), "List(3, 4)")
  test(eval("List(6, 3, 4).tail.head"), "3")
  testExc(eval("Nil.tail"), "empty list")
  testExc(eval("42.tail"), "not a list")
  test(eval("List(6, 3, 4).isEmpty"), "false")
  test(eval("Nil.isEmpty"), "true")
  testExc(eval("42.isEmpty"), "invalid operation")
  test(eval("List(6, 3, 4).length"), "3")
  test(eval("Nil.length"), "0")
  testExc(eval("42.length"), "not a list")
  test(eval("List(6, 3, 4).map(x => x + 1)"), "List(7, 4, 5)")
  test(eval("Nil.map(x => x + 1)"), "List()")
  testExc(eval("42.map(x => x + 1)"), "not a list")
  testExc(eval("List(6, 3, 4).map(42)"), "not a function")
  test(eval("List(5, 7).flatMap(x => List(x, x * 2))"), "List(5, 10, 7, 14)")
  test(eval("Nil.flatMap(x => List(x, x * 2))"), "List()")
  testExc(eval("42.flatMap(x => List(x, x * 2))"), "not a list")
  testExc(eval("List(5, 7).flatMap(42)"), "not a function")
  test(eval("List(3, 6, 7, 4, 2).filter(x => x % 2 == 0)"), "List(6, 4, 2)")
  testExc(eval("42.filter(x => x % 2 == 0)"), "not a list")
  testExc(eval("List(2, 4, 6).filter(x => x + 1)"), "not a boolean")
  testExc(eval("List(3, 6, 7, 4, 2).filter(42)"), "not a function")
  test(eval("for { x <- List(1, 2, 3); } yield x + 1"), "List(2, 3, 4)")
  test(eval("for { x <- List(1, 2, 3); if x % 2 == 1; } yield x"), "List(1, 3)")
  test(eval("for { x <- List(1, 2); y <- List(x); } yield y"), "List(1, 2)")
  testExc(eval("for { x <- 42; } yield x + 1"), "not a list")
  testExc(eval("for { x <- List(1, 2); y <- x; } yield y"), "not a list")
  test(eval("(1, 2, 3)"), "(1, 2, 3)")
  test(eval("(6, 9, 4)._2"), "9")
  testExc(eval("List(6, 9, 4)._2"), "not a tuple")
  testExc(eval("(1, 2, 3)._4"), "out of bounds")
  test(eval("val x = 42; x + 1"), "43")
  test(eval("val x = 2; val x = 3; x + 1"), "4")
  test(eval("{ val x = 1; x + 1 } + { val x = 2; x + 1 }"), "5")
  testExc(eval("{ val x = 42; x + 1 } + x"), "free identifier")
  test(eval("x => x + 1"), "<function>")
  test(eval("() => x"), "<function>")
  test(eval("(x => x + 2)(3)"), "5")
  test(eval("val addN = n => m => n + m; val add3 = addN(3); add3(4)"), "7")
  test(eval("val g = () => 1 + 2; g()"), "3")
  test(eval("val f = (a, b, c) => a + b + c; f(1, 2, 3)"), "6")
  test(eval("val x = 42; val f = y => x + y; val x = 3; f(1)"), "43")
  testExc(eval("42(true)"), "not a function")
  test(eval("def f(x) = x * 2; def g(y) = f(y + 1); g(3)"), "8")
  test(eval("def g(y) = f(y + 1); def f(x) = x * 2; g(3)"), "8")
  test(eval("def sum(x) = if (x < 1) 0 else x + sum(x - 1); sum(10)"), "55")
  test(eval("def mod(x, y) = if (x < y) x else mod(x - y, y); mod(42, 5)"), "2")
  testExc(eval("{ def f(x) = x + 1; f(1) } + f(42)"), "free identifier")
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
  val expr11 = """
    def isEven(x) = x == 0 || if (x > 0) isOdd(x - 1) else isOdd(x + 1);
    def isOdd(x) = if (x > 0) isEven(x - 1) else isEven(x + 1);
    isEven(532)
  """
  test(eval(expr11), "true")
  val expr12 = """
    def fold(list, default, fun) =
      def aux(prev, list) =
        if (list.isEmpty) prev
        else aux(fun(prev, list.head), list.tail);
      aux(default, list);
    val sum = l => fold(l, 0, (x, y) => x + y);
    sum(List(1, 2, 3, 4, 5))
  """
  test(eval(expr12), "15")
  val expr13 = """
    def fill(x, n) = if (n == 0) Nil else x :: fill(x, n - 1);
    def fold(list, default, fun) =
      def aux(prev, list) =
        if (list.isEmpty) prev
        else aux(fun(prev, list.head), list.tail);
      aux(default, list);
    fold(fill(1, 10), List(0), (prev, cur) => prev.head + cur :: prev)
  """
  test(eval(expr13), "List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)")
  val expr14 = """
    def isPrime(n) =
      def aux(k) =
        if (k * k > n) true
        else if (n % k == 0) false
        else aux(k + 1);
      if (n < 2) false
      else aux(2);
    def factorize(n) =
      if (isPrime(n)) n :: Nil
      else
        def aux(m) =
          if (n % m == 0) m :: factorize(n / m)
          else aux(m + 1);
        aux(2);
    factorize(936)
  """
  test(eval(expr14), "List(2, 2, 2, 3, 3, 13)")
  val expr15 = """
    val mkRec = body => {
      val fX = fY => {
        val f = x => fY(fY)(x);
        body(f)
      };
      fX(fX)
    };
    val sum = mkRec(sum => n => {
      if (n < 1) 0
      else n + sum(n - 1)
    });
    sum(10)
  """
  test(eval(expr15), "55")
  val expr16 = """
    val mkRec = body => {
      val fX = fY => {
        val f = x => fY(fY)(x);
        body(f)
      };
      fX(fX)
    };
    val fac = mkRec(fac => n => if (n < 1) 1 else n * fac(n - 1));
    fac(10)
  """
  test(eval(expr16), "3628800")
  val expr17 = """
    def range(from, to) =
      if (from > to) Nil
      else from :: range(from + 1, to);
    range(1, 5)
      .filter(x => x % 2 == 0)
      .flatMap(x => range(1, 5)
        .filter(y => y % 2 == 1)
        .map(y => x * y))
  """
  test(eval(expr17), "List(2, 6, 10, 4, 12, 20)")
  val expr18 = """
    def range(from, to) =
      if (from > to) Nil
      else from :: range(from + 1, to);
    for {
      x <- range(1, 5);
      if x % 2 != 1;
      y <- range(1, 5);
      if y % 2 != 0;
    } yield x * y
  """
  test(eval(expr18), "List(2, 6, 10, 4, 12, 20)")
  val expr19 = """
    val f = (x, y) => List(x, y);
    List(f(), f(1), f(1, 2), f(1, 2, 3))
  """
  test(eval(expr19), "List(List((), ()), List(1, ()), List(1, 2), List(1, 2))")
  val expr20 = """
    def f(x, y) = List(x, y);
    List(f(), f(1), f(1, 2), f(1, 2, 3))
  """
  test(eval(expr20), "List(List((), ()), List(1, ()), List(1, 2), List(1, 2))")

  /* Write your own tests */
}
