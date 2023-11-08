package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Cont.*

  type BOp = (BigInt, BigInt) => BigInt
  def numBOp(x: String)(op: BOp)(l: Value, r: Value): Value = (l, r) match
    case (NumV(l), NumV(r)) => NumV(op(l, r))
    case (l, r) => error(s"invalid operation: ${l.str} $x ${r.str}")

  val numAdd: (Value, Value) => Value = numBOp("+")(_ + _)
  val numMul: (Value, Value) => Value = numBOp("*")(_ * _)

  def interpCPS(expr: Expr, env: Env, k: Value => Value): Value = expr match
    case Num(n)         => k(NumV(n))
    case Add(l, r)      => interpCPS(l, env, {
      lv => interpCPS(r, env, {
        rv => k(numAdd(lv, rv))
      })
    })
    case Mul(l, r)      => interpCPS(l, env, {
      lv => interpCPS(r, env, {
        rv => k(numMul(lv, rv))
      })
    })
    case Id(x)          => k(env.getOrElse(x, error(s"free identifier: $x")))
    case Fun(p, b)      => k(CloV(p, b, env))
    case App(f, a)      => interpCPS(f, env, fv => fv match
      case CloV(p, b, fenv) => interpCPS(a, env, av => interpCPS(b, fenv + (p -> av), k))
      case v => error(s"not a function: ${v.str}")
    )
  
  def reduce(k: Cont, s: Stack): (Cont, Stack) = (k, s) match
    case (EvalK(env, expr, k), s)             => expr match
      case Add(l, r)  => (EvalK(env, l, EvalK(env, r, AddK(k))), s)
      case Mul(l, r)  => (EvalK(env, l, EvalK(env, r, MulK(k))), s)
      case App(f, a)  => (EvalK(env, f, EvalK(env, a, AppK(k))), s)
      case Id(x)      => (k, env.getOrElse(x, error(s"free identifier: $x")) :: s)
      case Fun(p, b)  => (k, CloV(p, b, env) :: s)
      case Num(n)     => (k, NumV(n) :: s)
    case (AddK(k), r :: l :: s)               => (k, numAdd(l, r) :: s)
    case (MulK(k), r :: l :: s)               => (k, numMul(l, r) :: s)
    case (AppK(k), v2 :: v1 :: s)  => v1 match
      case CloV(p, b, env)  => (EvalK(env + (p -> v2), b, k), s)
      case v                => error(s"not a function: ${v.str}")
}
