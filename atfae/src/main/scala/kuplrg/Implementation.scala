package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def lookupVar(tenv: TypeEnv, x: String): Type =
    tenv.vars.getOrElse(x, error(s"free identifier: $x"))
  def lookupType(tenv: TypeEnv, x: String): Map[String, List[Type]] =
    tenv.tys.getOrElse(x, error(s"unknown type: $x"))

  def mustSame(lty: Type, rty: Type): Unit =
    if(lty != rty) error(s"type error: ${lty.str} != ${rty.str}")

  def mustValid(ty: Type, tenv: TypeEnv): Type = ty match
    case NumT => NumT
    case BoolT => BoolT
    case ArrowT(ptys, rty) =>
      ArrowT(ptys.map(mustValid(_, tenv)), mustValid(rty, tenv))
    case NameT(tn) =>
      if(!tenv.tys.contains(tn)) error(s"invalid type name: $tn")
      NameT(tn)

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
      typeCheck(b, tenv.addVar(x, typeCheck(i, tenv)))
    case Id(x)                      => lookupVar(tenv, x)
    case Fun(ps, b) =>
      val ptys = ps.map(_.ty)
      for (pty <- ptys) mustValid(pty, tenv)
      val rty = typeCheck(b, tenv.addVars(ps.map(p => p.name -> p.ty)))
      ArrowT(ptys, rty)
    case Rec(x, ps, rty, b, s)  =>
      val ptys = ps.map{case Param(n, ty) => mustValid(ty, tenv)}
      val fty = ArrowT(ptys, mustValid(rty, tenv))
      mustSame(typeCheck(b, tenv.addVars(ps.map{case Param(n, ty) => (n, ty)}).addVar(x, fty)), rty)
      typeCheck(s, tenv.addVar(x, fty))
    case App(f, args)                => typeCheck(f, tenv) match
      case ArrowT(ps, bodyTy) =>
        args.zip(ps).map{case (e ,t) => mustSame(typeCheck(e, tenv), t)}
        bodyTy
      case ty                 => error(s"not a function type: ${ty.str}")
    case If(cond, thenE, elseE)     =>
      mustSame(typeCheck(cond, tenv), BoolT)
      val thenTy = typeCheck(thenE, tenv)
      val elseTy = typeCheck(elseE, tenv)
      mustSame(thenTy, elseTy)
      thenTy
    case TypeDef(tn, ws, body) => 
      if (tenv.tys.contains(tn)) error(s"already defined type: $tn")
      val newTEnv = tenv.addType(tn, ws.map(w => w.name -> w.ptys).toMap)
      for (w <- ws; pty <- w.ptys) mustValid(pty, newTEnv)
      mustValid(typeCheck(body, newTEnv.addVars(ws.map(w => w.name -> ArrowT(w.ptys, NameT(tn))))), tenv)
    case Match(expr, cs) => typeCheck(expr, tenv) match
      case NameT(tn) =>
        val ts = lookupType(tenv, tn)
        val xs = cs.map(_.name).toSet
        if(ts.keySet != xs || xs.size != cs.length) error("invalid case")
        cs.map { case MatchCase(x, ps, b) =>
          typeCheck(b, tenv.addVars(ps zip ts(x)))
        }.reduce((lty, rty) => {mustSame(lty, rty); lty})
      case _ => error("not a variant")

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

  def arityCheck(l: BigInt, r: BigInt): BigInt =
    if (l != r) error(s"not matching arity")
    l

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
    case Fun(ps, b)                 => CloV(ps.map(_.name), b, () => env)
    case Rec(x, ps, rty, b, s)      =>
      lazy val newEnv: Env = env + (x -> CloV(ps.map(_.name), b, () => newEnv))
      interp(s, newEnv)
    case App(f, args)               => interp(f, env) match
      case CloV(ps, b, fenv)  => interp(b, fenv() ++ ps.zip(args.map(interp(_, env))))
      case ConstrV(name)      => VariantV(name, args.map(interp(_, env)))
      case v                  => error(s"not a function: ${v.str}")
    case If(cond, thenE, elseE)     => interp(cond, env) match
      case BoolV(true)  => interp(thenE, env)
      case BoolV(false) => interp(elseE, env)
      case v            => error(s"not a boolean: ${v.str}")
    case TypeDef(name, varts, body) =>
      interp(body, env ++ varts.map(w => w.name -> ConstrV(w.name)))
    case Match(expr, mcases)        => interp(expr, env) match
      case VariantV(wname, vs) => mcases.find(_.name == wname) match
        case Some(MatchCase(_, ps, b)) =>
          arityCheck(ps.length, vs.length)
          interp(b, env ++ (ps zip vs))
        case None => error(s"no such case: $wname")
      case v => error(s"not a variant: ${v.str}")

}
