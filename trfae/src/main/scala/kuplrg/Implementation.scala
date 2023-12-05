package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def lookup(tenv: TypeEnv, x: String): Type =
    tenv.getOrElse(x, error(s"free identifier: $x"))

  def mustSame(lty: Type, rty: Type): Unit =
    if(lty != rty) error(s"type error: ${lty.str} != ${rty.str}")

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
    case Num(n)                     => NumT
    case Bool(b)                    => BoolT
    case Add(l, r)                  => 
      mustSame(typeCheck(l, tenv), NumT)
      mustSame(typeCheck(r, tenv), NumT)
      NumT
    case Mul(l, r)                  =>
      mustSame(typeCheck(l, tenv), NumT)
      mustSame(typeCheck(r, tenv), NumT)
      NumT
    case Div(l, r)                  =>
      mustSame(typeCheck(l, tenv), NumT)
      mustSame(typeCheck(r, tenv), NumT)
      NumT
    case Mod(l, r)                  =>
      mustSame(typeCheck(l, tenv), NumT)
      mustSame(typeCheck(r, tenv), NumT)
      NumT
    case Eq(l, r)                   =>
      mustSame(typeCheck(l, tenv), NumT)
      mustSame(typeCheck(r, tenv), NumT)
      BoolT
    case Lt(l, r)                   =>
      mustSame(typeCheck(l, tenv), NumT)
      mustSame(typeCheck(r, tenv), NumT)
      BoolT
    case Val(x, i, b)               => 
      typeCheck(b, tenv + (x -> typeCheck(i, tenv)))
    case Id(x)                      => lookup(tenv, x)
    case Fun(p, pty, b)             =>
      val bodyTy = typeCheck(b, tenv + (p -> pty))
      ArrowT(pty, bodyTy) 
    case Rec(x, p, pty, rty, b, s)  =>
      val fty = ArrowT(pty, rty)
      mustSame(typeCheck(b, tenv + (x -> fty) + (p -> pty)), rty)
      typeCheck(s, tenv + (x -> fty))
    case App(f, arg)                => typeCheck(f, tenv) match
      case ArrowT(paramTy, bodyTy) =>
        mustSame(typeCheck(arg, tenv), paramTy)
        bodyTy
      case ty => error(s"not a function type: ${ty.str}")
    case If(cond, thenE, elseE)     =>
      mustSame(typeCheck(cond, tenv), BoolT)
      val thenTy = typeCheck(thenE, tenv)
      val elseTy = typeCheck(elseE, tenv)
      mustSame(thenTy, elseTy)
      thenTy

  type BOp = (BigInt, BigInt) => BigInt
  type NCOp = (BigInt, BigInt) => Boolean

  def numBOp(x: String)(op: BOp)(l: Value, r: Value): Value = (l, r) match
    case (NumV(l), NumV(r)) => NumV(op(l, r))
    case (l, r) => error(s"invalid operation: ${l.str} $x ${r.str}")
  def numCOp(x: String)(op: NCOp)(l: Value, r: Value): Value = (l, r) match
    case (NumV(l), NumV(r)) => BoolV(op(l, r))
    case (l, r) => error(s"invalid operation: ${l.str} $x ${r.str}")
  
  def lookup(env: Env, x: String): Value =
    env.getOrElse(x, error(s"free identifier: $x"))

  val numAdd: (Value, Value) => Value = numBOp("+")(_ + _)
  val numMul: (Value, Value) => Value = numBOp("*")(_ * _)
  val numDiv: (Value, Value) => Value = numBOp("/")(_ / _)
  val numMod: (Value, Value) => Value = numBOp("%")(_ % _)
  val numEq: (Value, Value) => Value = numCOp("==")(_ == _)
  val numLt: (Value, Value) => Value = numCOp("<")(_ < _)

  def interp(expr: Expr, env: Env): Value = expr match
    case Num(n)                     => NumV(n)
    case Bool(b)                    => BoolV(b)
    case Add(l, r)                  => numAdd(interp(l, env), interp(r, env))
    case Mul(l, r)                  => numMul(interp(l, env), interp(r, env))
    case Div(l, r)                  => interp(r, env) match
      case NumV(0)  => error(s"invalid operation: ${l.str} / ${r.str}")
      case v        => numDiv(interp(l, env), interp(r, env))
    case Mod(l, r)                  => interp(r, env) match
      case NumV(0)  => error(s"invalid operation: ${l.str} % ${r.str}")
      case v        => numMod(interp(l, env), interp(r, env))
    case Eq(l, r)                   => numEq(interp(l, env), interp(r,env))
    case Lt(l, r)                   => numLt(interp(l, env), interp(r,env))
    case Val(n, i, b)               => interp(b, env + (n -> interp(i, env)))
    case Id(x)                      => lookup(env, x)
    case Fun(p, pty, b)             => CloV(p, b, () => env)
    case Rec(x, p, pty, rty, b, s)  =>
      lazy val newEnv: Env = env + (x -> CloV(p, b, () => newEnv))
      interp(s, newEnv)
    case App(f, arg)                => interp(f, env) match
      case CloV(p, b, fenv) => interp(b, fenv() + (p -> interp(arg, env)))
      case v                => error(s"not a function: ${v.str}")
    case If(cond, thenE, elseE)     => interp(cond, env) match
      case BoolV(true)  => interp(thenE, env)
      case BoolV(false) => interp(elseE, env)
      case v            => error(s"not a boolean: ${v.str}")
  

}
