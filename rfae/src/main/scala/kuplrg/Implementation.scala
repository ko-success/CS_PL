package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  type BOp = (BigInt, BigInt) => BigInt
  type NCOp = (BigInt, BigInt) => Boolean

  def numBOp(x: String)(op: BOp)(l: Value, r: Value): Value = (l, r) match
    case (NumV(l), NumV(r)) => NumV(op(l, r))
    case (l, r) => error(s"invalid operation: ${l.str} $x ${r.str}")
  def numCOp(x: String)(op: NCOp)(l: Value, r: Value): Value = (l, r) match
    case (NumV(l), NumV(r)) => BoolV(op(l, r))
    case (l, r) => error(s"invalid operation: ${l.str} $x ${r.str}")
  
  val numAdd: (Value, Value) => Value = numBOp("+")(_ + _)
  val numMul: (Value, Value) => Value = numBOp("*")(_ * _)
  val numDiv: (Value, Value) => Value = numBOp("/")(_ / _)
  val numMod: (Value, Value) => Value = numBOp("%")(_ % _)
  val numEq: (Value, Value) => Value = numCOp("==")(_ == _)
  val numLt: (Value, Value) => Value = numCOp("<")(_ < _)

  def interp(expr: Expr, env: Env): Value = expr match
    case Num(n)           => NumV(n)
    case Bool(b)          => BoolV(b)
    case Id(x)            => env.getOrElse(x, error(s"free identifier: $x"))
    case Add(l, r)        => numAdd(interp(l, env), interp(r, env))
    case Mul(l, r)        => numMul(interp(l, env), interp(r, env))
    case Div(l, r)        => interp(r, env) match
      case NumV(0)               => error(s"invalid operation: ${l.str} / ${r.str}")
      case v                    => numDiv(interp(l, env), interp(r, env))
    case Mod(l, r)        => interp(r, env) match
      case NumV(0)               => error(s"invalid operation: ${l.str} % ${r.str}")
      case v                    => numMod(interp(l, env), interp(r, env))
    case Eq(l, r)         => numEq(interp(l, env), interp(r,env))
    case Lt(l, r)         => numLt(interp(l, env), interp(r,env))
    case Fun(p, b)        => CloV(p, b, () => env)
    case Rec(n, p, b, s)  =>
      lazy val newEnv: Env = env + (n -> CloV(p, b, () => newEnv))
      interp(s, newEnv)
    case App(f, e)        => interp(f, env) match
      case CloV(p, b, fenv)     => interp(b, fenv() + (p -> interp(e, env)))
      case v                    => error(s"not a function: ${v.str}")
    case If(c, t, e)      => interp(c, env) match
      case BoolV(true)          => interp(t, env)
      case BoolV(false)         => interp(e, env)
      case v                    => error(s"not a boolean: ${v.str}")
}