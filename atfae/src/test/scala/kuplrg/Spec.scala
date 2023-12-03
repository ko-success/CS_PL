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
  test(eval("() => 42"), "<function>: () => Number")
  test(eval("(x: Number) => x"), "<function>: Number => Number")
  test(eval("(x: Boolean) => x"), "<function>: Boolean => Boolean")
  test(eval(
    "(x: Number, y: Number) => x + y"
  ), "<function>: (Number, Number) => Number")
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
    "(f: Number => Boolean, x: Number) => f(x)"
  ), "<function>: (Number => Boolean, Number) => Boolean")
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
  test(eval(
    "def twice(f: Number => Number, n: Number): Number = f(f(n));" +
    "twice((x: Number) => x + 1, 0)"
  ), "2: Number")
  test(eval(
    "def myIf(c: Boolean, t: Number, e: Number): Number = if (c) t else e;" +
    "myIf(true, 1, 2)"
  ), "1: Number")
  val shape = "enum Shape { case Square(Number) case Rect(Number, Number) }"
  // unknown variant: Circle
  testExc(eval(shape + "Circle"))
  // arity mismatch: 1 != 2
  testExc(eval(shape + "Square(10, 10)"))
  // arity mismatch: 2 != 1
  testExc(eval(shape + "Rect(3)"))
  test(eval(
    shape +
    "Square(10) match { case Square(n) => n * n; case Rect(w, h) => w * h }"
  ), "100: Number")
  test(eval(
    shape +
    "Rect(3, 5) match { case Square(n) => n * n; case Rect(w, h) => w * h }"
  ), "15: Number")
  // no such case: Rect
  testExc(eval(shape + "Rect(3, 5) match { case Square(n) => n * n }"))
  val natHelper = """
    enum Nat {
      case Zero()
      case Succ(Nat)
    }
    def nat2num(n: Nat): Number = n match {
      case Zero() => 0
      case Succ(n) => 1 + nat2num(n)
    }
    def num2nat(n: Number): Nat = {
      if (n <= 0) Zero()
      else Succ(num2nat(n - 1))
    }
    def add(n: Nat, m: Nat): Nat = n match {
      case Zero() => m
      case Succ(n) => Succ(add(n, m))
    }
    def mul(n: Nat, m: Nat): Nat = n match {
      case Zero() => Zero()
      case Succ(n) => add(m, mul(n, m))
    }
    def pow(n: Nat, m: Nat): Nat = m match {
      case Zero() => Succ(Zero())
      case Succ(m) => mul(n, pow(n, m))
    }
  """
  test(eval(natHelper + "nat2num(Succ(Succ(Succ(Zero()))))"), "3: Number")
  test(eval(natHelper + "nat2num(add(num2nat(2), num2nat(3)))"), "5: Number")
  test(eval(natHelper + "nat2num(mul(num2nat(2), num2nat(3)))"), "6: Number")
  test(eval(natHelper + "nat2num(pow(num2nat(2), num2nat(3)))"), "8: Number")
  val listHelper = """
    enum List {
      case Nil()
      case Cons(Number, List)
    }
    def sum(l: List): Number = l match {
      case Nil() => 0
      case Cons(x, xs) => x + sum(xs)
    }
    def map(l: List, f: Number => Number): List = l match {
      case Nil() => Nil()
      case Cons(x, xs) => Cons(f(x), map(xs, f))
    }
    def filter(l: List, f: Number => Boolean): List = l match {
      case Nil() => Nil()
      case Cons(x, xs) => if (f(x)) Cons(x, filter(xs, f)) else filter(xs, f)
    }
    def append(l1: List, l2: List): List = l1 match {
      case Nil() => l2
      case Cons(x, xs) => Cons(x, append(xs, l2))
    }
  """
  test(eval(
    listHelper + "sum(Nil())"
  ), "0: Number", weight = 2)
  test(eval(
    listHelper + "sum(Cons(1, Cons(2, Cons(3, Nil()))))"
  ), "6: Number", weight = 2)
  test(eval(
    listHelper +
    "sum(map(Cons(1, Cons(2, Cons(3, Nil()))), (x: Number) => x * x))"
  ), "14: Number", weight = 2)
  test(eval(
    listHelper +
    "sum(filter(Cons(1, Cons(2, Cons(3, Nil()))), (x: Number) => x % 2 == 1))"
  ), "4: Number", weight = 2)
  test(eval(
    listHelper +
    "sum(append(Cons(1, Cons(2, Cons(3, Nil()))), Cons(4, Cons(5, Nil()))))"
  ), "15: Number", weight = 2)
  val treeHelper = s"""
    $listHelper
    enum Tree {
      case Leaf(Number)
      case Node(Tree, Number, Tree)
    }
    def sumTree(t: Tree): Number = t match {
      case Leaf(x) => x
      case Node(l, x, r) => sumTree(l) + x + sumTree(r)
    }
    def mapTree(t: Tree, f: Number => Number): Tree = t match {
      case Leaf(x) => Leaf(f(x))
      case Node(l, x, r) => Node(mapTree(l, f), f(x), mapTree(r, f))
    }
    def flatten(t: Tree): List = t match {
      case Leaf(x) => Cons(x, Nil())
      case Node(l, x, r) => append(flatten(l), Cons(x, flatten(r)))
    }
    val tree = Node(Leaf(1), 2, Node(Leaf(3), 4, Leaf(5)))
  """
  test(eval(
    treeHelper + "sumTree(Leaf(42))"
  ), "42: Number", weight = 3)
  test(eval(
    treeHelper + "sumTree(tree)"
  ), "15: Number", weight = 3)
  test(eval(
    treeHelper + "sumTree(mapTree(tree, (x: Number) => x * x))"
  ), "55: Number", weight = 3)
  test(eval(
    treeHelper + "sum(flatten(tree))"
  ), "15: Number", weight = 3)
  test(eval(
    treeHelper + "sum(append(flatten(tree), Cons(6, Nil())))"
  ), "21: Number", weight = 3)
  val expr1 = """
    def gcd(a: Number): Number => Number = (b: Number) => {
      if (a == 0) b else gcd(b % a)(a)
    }
    gcd(432)(180)
  """
  test(eval(expr1), "36: Number")
  val expr2 = """
    def fact(n: Number): Number = if (n > 0) n * fact(n - 1) else 1
    fact(10)
  """
  test(eval(expr2), "3628800: Number")
  val expr3 = """
    def isPrime(n: Number): Boolean =
      def iter(k: Number): Boolean =
        if (k * k > n) true
        else if (n % k == 0) false
        else iter(k + 1)
      if (n < 2) false
      else iter(2)
    isPrime(1000003)
  """
  test(eval(expr3), "true: Boolean")
  val expr4 = """
    def sum(n: Number): Number =
      def iter(k: Number): Number => Number = (acc: Number) =>
        if (k > n) acc
        else iter(k + 1)(acc + k)
      iter(1)(0)
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
        }
      iter(0)(k)
    findKth((x: Number) => x % 3 == 2)(43)
  """
  test(eval(expr5), "128: Number")
  val expr6 = """
    def isPrime(n: Number): Boolean =
      def iter(k: Number): Boolean =
        if (k * k > n) true
        else if (n % k == 0) false
        else iter(k + 1)
      if (n < 2) false
      else iter(2)
    def findKth(f: Number => Boolean): Number => Number = (k: Number) =>
      def iter(n: Number): Number => Number = (k: Number) =>
        if (f(n)) {
          if (k == 1) n else iter(n + 1)(k - 1)
        } else {
          iter(n + 1)(k)
        }
      iter(0)(k)
    findKth(isPrime)(101)
  """
  test(eval(expr6), "547: Number")
  val expr7 = """
    val addN = (n: Number) => (x: Number) => x + n
    def isEven(x: Number): Boolean = x % 2 == 0
    def findKth(f: Number => Boolean): Number => Number = (k: Number) =>
      def iter(n: Number): Number => Number = (k: Number) =>
        if (f(n)) {
          if (k == 1) n else iter(n + 1)(k - 1)
        } else {
          iter(n + 1)(k)
        }
      iter(0)(k)
    findKth((n: Number) => isEven(addN(n)(1)))(23)
  """
  test(eval(expr7), "45: Number")
  val expr8 = """
    val sum = (n: Number) => {
      (map: Number => Number) => {
        (filter: Number => Boolean) => {
          def iter(k: Number): Number => Number = (acc: Number) =>
            if (k > n) acc
            else iter(k + 1)(acc + (if (filter(k)) map(k) else 0))
          iter(1)(0)
        }
      }
    }
    val square = (x: Number) => x * x
    val isOdd = (x: Number) => x % 2 == 1
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
          else iter(k + 1)(f(acc)(k))
        iter(from)(default)
    }
    val add = (x: Number) => (y: Number) => x + y
    fold(add)(0)(53)(78)
  """
  test(eval(expr9), "1703: Number")
  val expr10 = """
    enum T { case T(T => Number => Number) }
    val mkRec = (body: (Number => Number) => Number => Number) => {
      val fX = (fY: T) => {
        val f = (x: Number) => fY match { case T(fZ) => fZ(fY)(x) };
        body(f)
    };
    fX(T(fX)) };
    val sum = mkRec((sum: Number => Number) => (n: Number) =>
      if (n < 1) 0
      else n + sum(n + -1));
    sum(10)
  """
  test(eval(expr10), "55: Number")
  val expr11 = """
    enum A {
      case X(Number)
    }
    def get(a: A): Number = a match {
      case X(n) => n
    }
    enum A {
      case X(Boolean)
    }
    get(X(true))
  """
  testExc(eval(expr11), weight = 5)
  val expr12 = """
    val get = {
      enum A {
        case X(Number)
      }
      (a: A) => a match {
        case X(n) => n
      }
    }
    enum A {
      case X(Boolean)
    }
    get(X(true))
  """
  testExc(eval(expr12), weight = 5)

  /* Write your own tests */

}
