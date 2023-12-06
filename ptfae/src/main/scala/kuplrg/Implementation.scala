package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def lookupVar(name: String, tenv: TypeEnv): Type =
    tenv.vars.getOrElse(name, error(s"free identifier: $name"))
  def lookupType(name: String, tenv: TypeEnv): String =
    if (!tenv.tys.contains(name)) error(s"unknown type: $name")
    name

  def isSame(lty: Type, rty: Type): Boolean = (lty, rty) match
    case (NumT, NumT)                             => true
    case (ArrowT(lpty, lrty), ArrowT(rpty, rrty)) =>
      isSame(lpty, rpty) && isSame(lrty, rrty)
    case (VarT(lname), VarT(rname))               => lname == rname
    case (PolyT(lname, lty), PolyT(rname, rty))   =>
      isSame(lty, subst(rty, rname, VarT(lname)))
    case _                                        => false

  def mustSame(lty: Type, rty: Type): Unit =
    if(!isSame(lty, rty)) error(s"type mismatch: ${lty.str} != ${rty.str}")

  def mustValid(ty: Type, tenv: TypeEnv): Type = ty match
    case NumT               => NumT
    case ArrowT(pty, rty)   => ArrowT(mustValid(pty, tenv), mustValid(rty, tenv))
    case VarT(name)         => VarT(lookupType(name, tenv))
    case PolyT(name, ty)    => PolyT(name, mustValid(ty, tenv.addType(name)))

  def subst(bodyTy: Type, tyVar: String, insTy: Type): Type = bodyTy match
    case NumT             => NumT
    case ArrowT(pty, rty) => ArrowT(subst(pty, tyVar, insTy), subst(rty, tyVar, insTy))
    case VarT(name)       => 
      if (tyVar == name) insTy
      else VarT(name)
    case PolyT(name, ty)  => 
      if (tyVar == name) PolyT(name, ty)
      else PolyT(name, subst(ty, tyVar, insTy))

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
    case Num(number: BigInt)                        => NumT
    case Add(left: Expr, right: Expr)               => 
      mustSame(typeCheck(left, tenv), NumT)
      mustSame(typeCheck(right, tenv), NumT)
      NumT
    case Mul(left: Expr, right: Expr)               =>
      mustSame(typeCheck(left, tenv), NumT)
      mustSame(typeCheck(right, tenv), NumT)
      NumT
    case Val(name: String, init: Expr, body: Expr)  =>
      typeCheck(body, tenv.addVar(name, typeCheck(init, tenv)))
    case Id(name: String)                           =>
      lookupVar(name, tenv)
    case Fun(param: String, ty: Type, body: Expr)   =>
      mustValid(ty, tenv)
      ArrowT(ty, typeCheck(body, tenv.addVar(param, ty)))
    case App(fun: Expr, arg: Expr)                  => typeCheck(fun, tenv) match
      case ArrowT(paramTy, retTy) =>
        mustSame(typeCheck(arg, tenv), paramTy)
        retTy
      case t => error(s"not a function type: ${t.str}")
    case TypeAbs(name: String, body: Expr)          =>
      if(tenv.tys.contains(name)) error(s"already defined type: $name")
      PolyT(name, typeCheck(body, tenv.addType(name)))
    case TypeApp(expr: Expr, ty: Type)              => typeCheck(expr, tenv) match
      case PolyT(name, bodyTy) => subst(bodyTy, name, mustValid(ty, tenv))
      case t => error(s"not a polymorphic type: ${t.str}")

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
    case Id(name: String)                           => lookup(name, env)
    case Fun(param: String, ty: Type, body: Expr)   => CloV(param, body, env)
    case App(fun: Expr, arg: Expr)                  => interp(fun, env) match
      case CloV(p, b, fenv) => interp(b, fenv + (p -> interp(arg, env)))
      case v                => error(s"not a function: ${v.str}")
    case TypeAbs(name: String, body: Expr)          => TypeAbsV(name, body, env)
    case TypeApp(expr: Expr, ty: Type)              => interp(expr, env) match
      case TypeAbsV(name, body, fenv) => interp(body, fenv)
      case v                          => error(s"not a type abstraction: ${v.str}")
  }
