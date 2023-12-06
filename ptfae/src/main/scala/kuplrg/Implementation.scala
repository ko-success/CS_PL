package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def mustSame(lty: Type, rty: Type): Unit =
  if(lty != rty) error(s"type error: ${lty.str} != ${rty.str}")

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
    case Num(number: BigInt)                        =>
    case Add(left: Expr, right: Expr)               =>
    case Mul(left: Expr, right: Expr)               =>
    case Val(name: String, init: Expr, body: Expr)  =>
    case Id(name: String)                           =>
    case Fun(param: String, ty: Type, body: Expr)   =>
    case App(fun: Expr, arg: Expr)                  =>
    case TypeAbs(name: String, body: Expr)          =>
    case TypeApp(expr: Expr, ty: Type)              =>

  type BOp = (BigInt, BigInt) => BigInt

  def numBOp(x: String)(op: BOp)(l: Value, r: Value): Value = (l, r) match
    case (NumV(l), NumV(r)) => NumV(op(l, r))
    case (l, r) => error(s"invalid operation: ${l.str} $x ${r.str}")

  def lookup(x: String, env: Env): Value = env.getOrElse(x, error(s"free identifier: $x"))

  val numAdd: (Value, Value) => Value = numBOp("+")(_ + _)
  val numMul: (Value, Value) => Value = numBOp("*")(_ * _)

  def interp(expr: Expr, env: Env): Value = expr match
    case Num(number: BigInt)                        => NumV(number)
    case Add(left: Expr, right: Expr)               => numAdd(interp(left, env), interp(right, env))
    case Mul(left: Expr, right: Expr)               => numMul(interp(left, env), interp(right, env))
    case Val(name: String, init: Expr, body: Expr)  => interp(body, env + (name -> interp(init, env)))
    case Id(name: String)                           => lookup(name)
    case Fun(param: String, ty: Type, body: Expr)   => CloV(param, body, env)
    case App(fun: Expr, arg: Expr)                  => interp(fun, env) match
      case CloV(p, b, fenv) => interp(b, fenv + (p -> interp(arg, env)))
      case v                => error(s"not a function: ${v.str}")
    case TypeAbs(name: String, body: Expr)          => TypeAbsV(name, body, env)
    case TypeApp(expr: Expr, ty: Type)              => interp(expr, env) match
      case TypeAbsV(name, body, fenv) => interp(body, fenv)
      case v                          => error(s"not a type abstraction: ${v.str}")
  }
