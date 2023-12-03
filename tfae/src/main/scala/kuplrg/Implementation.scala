package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def mustSame(lty: Type, rty: Type): Unit =
	if(lty != rty) error(s"type error: ${lty.str} != ${rty.str}")

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
    case Num(num)                   => NumT
    case Add(left, right)           =>
      mustSame(typeCheck(left, tenv), NumT)
      mustSame(typeCheck(right, tenv), NumT)
        NumT
    case Mul(left, right)           =>
      mustSame(typeCheck(left, tenv), NumT)
      mustSame(typeCheck(right, tenv), NumT)
      NumT
    case Id(x)                      => tenv.getOrElse(x, error(s"free identifier: $x"))
    case Fun(param, paramTy, body)  =>
      val bodyTy = typeCheck(body, tenv + (param -> paramTy))
      ArrowT(paramTy, bodyTy)
    case App(fun, arg)              => typeCheck(fun, tenv) match
      case ArrowT(paramTy, bodyTy) =>
        mustSame(typeCheck(arg, tenv), paramTy)
        bodyTy
      case ty => error(s"not a function type: ${ty.str}")


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

}
