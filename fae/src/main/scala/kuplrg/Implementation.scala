package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  type BOp = (BigInt, BigInt) => BigInt
  def numBOp(x: String)(op: BOp)(l: Value, r: Value): Value = (l, r) match
    case (NumV(l), NumV(r)) => NumV(op(l, r))
    case (l, r) => error(s"invalid operation: ${l.str} $x ${r.str}")

  val numAdd: (Value, Value) => Value = numBOp("+")(_ + _)
  val numMul: (Value, Value) => Value = numBOp("*")(_ * _)

  def interp(expr: Expr, env: Env): Value = expr match
    case Num(n)         => NumV(n)
    case Add(l, r)      => numAdd(interp(l, env), interp(r, env))
    case Mul(l, r)      => numMul(interp(l, env), interp(r, env))
    case Id(x)          => env.getOrElse(x, error(s"free identifier: $x"))
    case Fun(p, b)      => CloV(p, b, env)
    case App(f, e)      => interp(f, env) match
      case CloV(p, b, fenv) => interp(b, fenv + (p -> interp(e, env)))
      case v => error(s"not a function: ${v.str}")

  def interpDS(expr: Expr, env: Env): Value = expr match
    case Num(n)         => NumV(n)
    case Add(l, r)      => numAdd(interpDS(l, env), interpDS(r, env))
    case Mul(l, r)      => numMul(interpDS(l, env), interpDS(r, env))
    case Id(x)          => env.getOrElse(x, error(s"free identifier: $x"))
    case Fun(p, b)      => CloV(p, b, env)
    case App(f, e)      => interpDS(f, env) match
      case CloV(p, b, _) => 
        println(env)
        val fenv = env + (p -> interpDS(e, env))
        println(fenv)
        interpDS(b, fenv)
      case v => error(s"not a function: ${v.str}")
}
