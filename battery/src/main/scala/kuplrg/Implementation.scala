package kuplrg

object Implementation extends Template {

  import Expr.*
  import RecDef.*
  import Value.*
  import Type.*
  import TypeInfo.*


  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
    case EUnit                                        => UnitT
    case ENum(number: BigInt)                         => NumT
    case EBool(bool: Boolean)                         => BoolT
    case EStr(string: String)                         => StrT
    case EId(name: String)                            => lookupVar(name, tenv)
    case EAdd(left: Expr, right: Expr)                => sameThisType(left, right, tenv, NumT, NumT)
    case EMul(left: Expr, right: Expr)                => sameThisType(left, right, tenv, NumT, NumT)
    case EDiv(left: Expr, right: Expr)                => sameThisType(left, right, tenv, NumT, NumT)
    case EMod(left: Expr, right: Expr)                => sameThisType(left, right, tenv, NumT, NumT)
    case EConcat(left: Expr, right: Expr)             => sameThisType(left, right, tenv, StrT, StrT)
    case EEq(left: Expr, right: Expr)                 =>
      isSame(typeCheck(left, tenv), typeCheck(right, tenv))
      BoolT
    case ELt(left: Expr, right: Expr)                 => sameThisType(left, right, tenv, NumT, BoolT)
    case ESeq(left: Expr, right: Expr)                =>
      val lty: Type = typeCheck(left, tenv)
      typeCheck(right, tenv)
    case EIf(cond: Expr, 
      thenExpr: Expr, 
      elseExpr: Expr)                                 =>
      mustSame(typeCheck(cond, tenv), BoolT)
      errorIfF(isSame(typeCheck(thenExpr, tenv), typeCheck(elseExpr, tenv)), "not equivalent type")
      typeCheck(thenExpr, tenv)
    case EVal(x: String, 
      tyOpt: Option[Type], 
      expr: Expr, body: Expr)                         => tyOpt match
      case Some(initTy) => 
        val initExprTy: Type = typeCheck(expr, tenv)
        errorIfF(isSame(initExprTy, initTy), "return types are not equivalent")
        typeCheck(body, tenv + (x -> initTy))
      case None         => typeCheck(body, tenv + (x -> typeCheck(expr, tenv)))
    case EFun(params: List[Param], body: Expr)        =>
      val paramTys: List[Type] = params.map(mustValid(_._2, tenv))
      // addvars 이게 맞나?
      val finTEnv: TypeEnv = tenv.addVars(params)
      ArrowT(paramTys, typeCheck(body, finTEnv))
    // function applications
    case EApp(fun: Expr, 
      tys: List[Type], args: List[Expr])              =>
    // mutually recursive definitions
    case ERecDefs(defs: List[RecDef], body: Expr)     =>
    // pattern matching
    case EMatch(expr: Expr, mcases: List[MatchCase])  =>
    case EExit(ty: Type, expr: Expr)                  =>
      mustSame(typeCheck(expr, tenv), StrT)
      mustValid(ty, tenv)
    case _  => error(s"error in type checking")

  def typeEnvUpdate(recDef: RecDef, typeEnv: TypeEnv) = recDef match
    // immutable lazy variable definition
    case LazyVal(
      name: String,
      ty: Type,
      init: Expr,
    ) =>
    // recursive function
    case RecFun(
      name: String,
      tvars: List[String],
      params: List[Param],
      rty: Type,
      body: Expr,
    ) =>
    // polymorphic algebraic data type
    case TypeDef(
      name: String,
      tvars: List[String],
      varts: List[Variant],
    ) =>

  def sameThisType(l: Expr, r: Expr, tenv: TypeEnv, checkTy: Type, retTy: Type): Type =
    mustSame(typeCheck(l, tenv), checkTy)
    mustSame(typeCheck(r, tenv), checkTy)
    retTy

  def mustSame(lty: Type, rty: Type): Unit =
    if(lty != rty) error(s"type error: ${lty.str} != ${rty.str}")

  def lookupVar(name: String, tenv: TypeEnv): Type =
    tenv.vars.getOrElse(name, error(s"free identifier: $name"))
  def lookupType(name: String, tenv: TypeEnv): TypeInfo =
    tenv.tys.getOrElse(name, error(s"unknown type: $name"))
    //if (!tenv.tys.contains(name)) error(s"unknown type: $name")

  def arityCheck(l: BigInt, r: BigInt): BigInt =
    if (l != r) error(s"not matching arity")
    l
  
  def errorIfF(b: Boolean, msg: String): Nothing = if(!b) error(msg)

  def mustValid(ty: Type, tenv: TypeEnv): Type = ty match
    case UnitT                          => UnitT
    case NumT                           => NumT
    case BoolT                          => BoolT
    case StrT                           => StrT
    case IdT(name, tys)                 => lookupType(name) match
      case TIVar                  => IdT(name)
      case TIAdt(tvars, variants) => IdT(name, tys.map(mustValid(_, tenv)))
    case ArrowT(tvars, paramTys, retTy) =>
      val newTEnv: TypeEnv = tenv.addTypeVars(tvars)
      ArrowT(tvars, paramTys.map(mustValid(_, newTEnv)), mustValid(retTy, tenv))

  def isSame(lty: Type, rty: Type): Boolean = (lty, rty) match
    case (UnitT, UnitT)                                       => true
    case (NumT, NumT)                                         => true
    case (BoolT, BoolT)                                       => true
    case (StrT, StrT)                                         => true
    case (IdT(lname, ltys), IdT(rname, rtys))                 => (ltys, rtys) match
      // alpha인지 t인지 tenv에서 안찾고 알려면 이방법 뿐인건가?
      case (Nil, Nil)               => lname == rname 
      case (h1 :: t1, h2 :: t2)     => 
        ltys.zip(rtys).foldLeft(ltys.length == rtys.length){ case (b, (lt, rt) => b && isSame(lt, rt)) }
      case t                        => false
    case (ArrowT(ltvs, lpts, lrty), ArrowT(rtvs, rpts, rrty)) =>
      // return type에 대해서는 is same 안해봐도 되는건가?
      val arityCheck: Boolean = (ltvs.length == rtvs.length) && (lpts.length == rpts.length)
      lpts.zip(rpts).foldLeft(arityCheck){ case (b, (lt, rt) => b && isSame(lt, subst(rt, rtvs, ltvs.map(IdT(_))))) }
    case _                                                    => false
  
  // 수정필요
  def subst(bodyTy: Type, tyVar: List[String], insTy: List[Type]): Type = bodyTy match
    case UnitT                          => UnitT
    case NumT                           => NumT
    case BoolT                          => BoolT
    case StrT                           => StrT
    case ArrowT(tvars, paramTys, retTy) => UnitT
    case IdT(name, tys)                 => UnitT
    // case ArrowT(pty, rty) => ArrowT(subst(pty, tyVar, insTy), subst(rty, tyVar, insTy))
    // case VarT(name)       => 
    //   if (tyVar == name) insTy
    //   else VarT(name)
    // case PolyT(name, ty)  => 
    //   if (tyVar == name) PolyT(name, ty)
    //   else PolyT(name, subst(ty, tyVar, insTy))


  val numAdd: (Value, Value) => Value = numBOp("+")(_ + _)
  val numMul: (Value, Value) => Value = numBOp("*")(_ * _)
  val numDiv: (Value, Value) => Value = numBOp("/")(_ / _)
  val numMod: (Value, Value) => Value = numBOp("%")(_ % _)
  val numLt: (Value, Value) => Value = numCOp("<")(_ < _)

  def interp(expr: Expr, env: Env): Value = expr match
    case EUnit                                        => UnitV
    case ENum(number: BigInt)                         => NumV(number)
    case EBool(bool: Boolean)                         => BoolV(bool)
    case EStr(string: String)                         => StrV(string)
    case EId(name: String)                            => lookup(name, env) match
      case ExprV(e, env)  => error(s"no ExprV")
      case v              => v
    case EAdd(left: Expr, right: Expr)                => numAdd(interp(left, env), interp(right, env))
    case EMul(left: Expr, right: Expr)                => numMul(interp(left, env), interp(right, env))
    case EDiv(left: Expr, right: Expr)                => interp(right, env) match
      case NumV(0)  => error(s"invalid operation: ${left.str} / 0")
      case rv       => numDiv(interp(left, env), rv)
    case EMod(left: Expr, right: Expr)                =>  interp(right, env) match
      case NumV(0)  => error(s"invalid operation: ${left.str} / 0")
      case rv       => numMod(interp(left, env), rv)
    case EConcat(left: Expr, right: Expr)             => stringConcat(interp(left, env), interp(right, env))
    case EEq(left: Expr, right: Expr)                 => BoolV(eq(interp(left, env), interp(right, env)))
    case ELt(left: Expr, right: Expr)                 => numLt(interp(left, env), interp(right, env))
    case ESeq(left: Expr, right: Expr)                =>
      val lv: Value = interp(left, env)
      interp(right, env)
    case EIf(cond: Expr, 
      thenExpr: Expr, 
      elseExpr: Expr)                                 => interp(cond, env) match
      case BoolV(true)  => interp(thenExpr, env)
      case BoolV(false) => interp(elseExpr, env)
      case _            => error(s"not a boolean")
    case EVal(x: String, 
      tyOpt: Option[Type], 
      expr: Expr, body: Expr)                         => 
      val initVal: Value = interp(expr, env)
      interp(body, env + (x -> initVal))
    case EFun(params: List[Param], body: Expr)        =>
      val parameters: List[String] = params.map(_.name)
      CloV(parameters, body, () => env)
    case EApp(fun: Expr, 
      tys: List[Type], args: List[Expr])              => 
      val argVals: List[Value] = args.map(interp(_, env))
      interp(fun, env) match
        case CloV(ps, b, fenv)  => interp(b, fenv() ++ ps.zip(argVals))
        case ConstrV(name)      => VariantV(name, argVals) 
        case v                  => error(s"not a function or variant: ${v.str}")
    // mutually recursive definitions
    case ERecDefs(defs: List[RecDef], body: Expr)     =>
    // 이거도대체 어떻게 짜는거지
      // lazy val finEnv: Env = defs.foldLeft(env){ case (e, d) => envUpdate(d, e, finEnv)}
      val finEnv = env
      interp(body, finEnv)
    case EMatch(expr: Expr, mcases: List[MatchCase])  => interp(expr, env) match
      case VariantV(name, values) => mcases.find(_.name == name) match
        case Some(MatchCase(_, params, body)) =>
          arityCheck(params.length, values.length)
          interp(body, env ++ (params zip values))
        case None => error(s"no such case: $name")
      case v                      => error(s"not a variant: ${v.str}")
    case EExit(ty: Type, expr: Expr)                  => error("exit")
    case v                                            => error(s"interpreter error: ${v.str}")

  type BOp = (BigInt, BigInt) => BigInt
  type NCOp = (BigInt, BigInt) => Boolean

  def numBOp(x: String)(op: BOp)(l: Value, r: Value): Value = (l, r) match
    case (NumV(l), NumV(r)) => NumV(op(l, r))
    case (l, r) => error(s"invalid operation: ${l.str} $x ${r.str}")
  def numCOp(x: String)(op: NCOp)(l: Value, r: Value): Value = (l, r) match
    case (NumV(l), NumV(r)) => BoolV(op(l, r))
    case (l, r) => error(s"invalid operation: ${l.str} $x ${r.str}")

  def lookup(x: String, env: Env): Value = env.getOrElse(x, error(s"free identifer: $x"))

  def arityCheck(l: BigInt, r: BigInt): BigInt =
    if (l != r) error(s"not matching arity")
    l

  def envUpdate(recDef: RecDef, curEnv: Env, updateEnv: Env): Env = recDef match
    case LazyVal(name: String, 
      ty: Type, 
      init: Expr,
    ) => curEnv + (name -> ExprV(init, () => updateEnv))
    case RecFun(name: String,
      tvars: List[String],
      params: List[Param],
      rty: Type,
      body: Expr,
    ) => curEnv + (name -> (CloV(params.map(_.name), body, () => updateEnv)))
    case TypeDef(
      name: String,
      tvars: List[String],
      varts: List[Variant],
    ) => 
      val vartNames: List[String] = varts.map(_.name)
      curEnv ++ vartNames.zip(vartNames.map(ConstrV(_)))

  def eq(l: Value, r: Value): Boolean = (l, r) match
    case (UnitV, UnitV)         => true
    case (NumV(l), NumV(r))     => l == r
    case (BoolV(l), BoolV(r))   => l == r
    case (StrV(l), StrV(r))     => l == r
    case (VariantV(lname, llist), VariantV(rname, rlist)) => 
      llist.zip(rlist).foldLeft(lname == rname){case (b, (l, r)) => b && eq(l, r)}
    case _                      => false

  def stringConcat(l: Value, r: Value): StrV = (l, r) match
    case (StrV(l), StrV(r)) => StrV(l + r)
    case _  => error(s"invalid operation")

}