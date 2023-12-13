package kuplrg

object Implementation extends Template {

  import Expr.*
  import RecDef.*
  import Value.*
  import Type.*
  import TypeInfo.*

  // type checker
  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
    case EUnit                                        => UnitT
    case ENum(number: BigInt)                         => NumT
    case EBool(bool: Boolean)                         => BoolT
    case EStr(string: String)                         => StrT
    case EId(name: String)                            => 
      // val v = lookupVar(name, tenv)
      // tenv.printTypeEnv
      // println(s"${name} is : ${v}")
      lookupVar(name, tenv)
    case EAdd(left: Expr, right: Expr)                => sameThisType(left, right, tenv, NumT, NumT)
    case EMul(left: Expr, right: Expr)                => sameThisType(left, right, tenv, NumT, NumT)
    case EDiv(left: Expr, right: Expr)                => sameThisType(left, right, tenv, NumT, NumT)
    case EMod(left: Expr, right: Expr)                => sameThisType(left, right, tenv, NumT, NumT)
    case EConcat(left: Expr, right: Expr)             => sameThisType(left, right, tenv, StrT, StrT)
    case EEq(left: Expr, right: Expr)                 =>
      errorIfF(isEquivType(typeCheck(left, tenv), typeCheck(right, tenv)), "can't check type equivalence")
      BoolT
    case ELt(left: Expr, right: Expr)                 => sameThisType(left, right, tenv, NumT, BoolT)
    case ESeq(left: Expr, right: Expr)                =>
      val lty: Type = typeCheck(left, tenv)
      typeCheck(right, tenv)
    case EIf(cond: Expr, 
      thenExpr: Expr, 
      elseExpr: Expr)                                 =>
      mustSame(typeCheck(cond, tenv), BoolT)
      errorIfF(isEquivType(typeCheck(thenExpr, tenv), typeCheck(elseExpr, tenv)), "not equivalent type")
      typeCheck(thenExpr, tenv)
    case EVal(x: String, 
      tyOpt: Option[Type], 
      expr: Expr, body: Expr)                         => tyOpt match
      case Some(initTy) => 
        val initExprTy: Type = typeCheck(expr, tenv)
        errorIfF(isEquivType(initExprTy, initTy), "return types are not equivalent")
        typeCheck(body, tenv.addVar(x, initTy))
      case None         => typeCheck(body, tenv.addVar(x, typeCheck(expr, tenv)))
    case EFun(params: List[Param], body: Expr)        =>
      val paramTys: List[Type] = params.map{ case p => isValidType(p.ty, tenv) }
      val finTEnv: TypeEnv = tenv.addVars(params.map{ case p => (p.name, p.ty) })
      ArrowT(Nil, paramTys, typeCheck(body, finTEnv))
    case EApp(fun: Expr, 
      tys: List[Type], args: List[Expr])              => typeCheck(fun, tenv) match
      case ArrowT(tvars, paramTys, retTy) => 
        val argTys: List[Type] = args.map(typeCheck(_, tenv))
        val equivCheck: List[Boolean] = argTys.zip(paramTys).map{ case t => isEquivType(t._1, subst(t._2, tvars, tys.map(isValidType(_, tenv)))) }
        errorIfF(equivCheck.foldLeft(paramTys.length == args.length){ case (l, r) => l && r}, "not equivalent types in App")
        subst(retTy, tvars, tys)
      case _                              => error("not a Arrow Type")
    case ERecDefs(defs: List[RecDef], body: Expr)     =>
      val finTEnv: TypeEnv = defs.foldLeft(tenv){ case (te, d) => typeEnvUpdate(d, te) }
      defs.map(isValidDef(_, finTEnv))
      isValidType(typeCheck(body, finTEnv), tenv)
    case EMatch(expr: Expr, mcases: List[MatchCase])  => typeCheck(expr, tenv) match
      case IdT(name, tys) => lookupType(name, tenv) match
        case TIAdt(tvars, variants) =>
          val mcaseNames: List[String] = mcases.map(_.name)
          if(mcaseNames.length != mcaseNames.toSet.size) error(s"duplicate case")
          val tenvs: List[TypeEnv] = mcases.map{
            case m => tenv.addVars(m.params.zip(variants.getOrElse(m.name, error(s"no matching variant")).map{ case p => subst(p.ty, tvars, tys) }))
          }
          val caseTys: List[Type] = mcases.map(_.body).zip(tenvs).map{ case tu => typeCheck(tu._1, tu._2) }
          if(caseTys.sliding(2).toList.foldLeft(mcases.length == variants.size){
            case (b, List(t1, t2))  => b && isEquivType(t1, t2)
            case (b, _)             => b && true}
          ) caseTys.head
          else error("not equivalent types")
        case t                      => error("not a ADT")
      case t              => error(s"not a IdT: ${t.str}")
    case EExit(ty: Type, expr: Expr)                  =>
      mustSame(typeCheck(expr, tenv), StrT)
      isValidType(ty, tenv)

  def typeEnvUpdate(recDef: RecDef, tenv: TypeEnv): TypeEnv = recDef match
    case LazyVal(
      name: String,
      ty: Type,
      init: Expr,
    ) => tenv.addVar(name, ty)
    case RecFun(
      name: String,
      tvars: List[String],
      params: List[Param],
      rty: Type,
      body: Expr,
    ) => tenv.addVar(name, ArrowT(tvars, params.map(_.ty), rty))
    case TypeDef(
      name: String,
      tvars: List[String],
      varts: List[Variant],
    ) => 
      val updateADT: TypeEnv = tenv.addTypeName(notContainTyVar(name, tenv), tvars, varts)
      updateADT.addVars(varts.map{ case v => (v.name, ArrowT(tvars, v.params.map(_.ty), IdT(name, tvars.map{ case n => IdT(n) }))) })

  // check well-type
  def isValidType(ty: Type, tenv: TypeEnv): Type = ty match
    case UnitT                          => UnitT
    case NumT                           => NumT
    case BoolT                          => BoolT
    case StrT                           => StrT
    case IdT(name, tys)                 => lookupType(name, tenv) match
      case TIVar                  => IdT(name)
      case TIAdt(tvars, variants) => IdT(name, tys.map(isValidType(_, tenv)))
    case ArrowT(tvars, paramTys, retTy) =>
      val newTEnv: TypeEnv = tenv.addTypeVars(tvars)
      ArrowT(tvars, paramTys.map(isValidType(_, newTEnv)), isValidType(retTy, newTEnv))

  def isValidDef(recDef: RecDef, tenv: TypeEnv): RecDef = recDef match
    case LazyVal(
      name: String,
      ty: Type,
      init: Expr,
    ) => 
      errorIfF(isEquivType(isValidType(ty, tenv), typeCheck(init, tenv)), "not well-typed definition")
      recDef
    case RecFun(
      name: String,
      tvars: List[String],
      params: List[Param],
      rty: Type,
      body: Expr,
    ) =>
      val finTEnv: TypeEnv = tenv.addTypeVars(tvars.map{ case name => notContainTyVar(name, tenv) })
      val paramTEnv: TypeEnv = finTEnv.addVars(params.map{ case p => (p.name, isValidType(p.ty, finTEnv)) })
      val evalRty: Type = typeCheck(body, paramTEnv)
      errorIfF(isEquivType(isValidType(rty, finTEnv), evalRty), "not equivalent types")
      RecFun(name, tvars, params, rty, body)
    case TypeDef(
      name: String,
      tvars: List[String],
      varts: List[Variant],
    ) =>
      val finTEnv: TypeEnv = tenv.addTypeVars(tvars.map{ case name => notContainTyVar(name, tenv) })
      TypeDef(name, tvars, varts.map{ case v => Variant(v.name, v.params.map{ case p => Param(p.name, isValidType(p.ty, finTEnv)) }) })

  // type equivalence
  def isEquivType(lty: Type, rty: Type): Boolean = (lty, rty) match
    case (UnitT, UnitT)                                       => true
    case (NumT, NumT)                                         => true
    case (BoolT, BoolT)                                       => true
    case (StrT, StrT)                                         => true
    case (IdT(lname, ltys), IdT(rname, rtys))                 => (ltys, rtys) match
      case (Nil, Nil)               => lname == rname 
      case (h1 :: t1, h2 :: t2)     => 
        ltys.zip(rtys).foldLeft(ltys.length == rtys.length){ case (b, (lt, rt)) => b && isEquivType(lt, rt) }
      case t                        => false
    case (ArrowT(ltvs, lpts, lrty), ArrowT(rtvs, rpts, rrty)) =>
      // return type에 대해서는 is same 안해봐도 되는건가?
      val arityCheck: Boolean = (ltvs.length == rtvs.length) && (lpts.length == rpts.length)
      lpts.zip(rpts).foldLeft(arityCheck){ case (b, (lt, rt)) => b && isEquivType(lt, subst(rt, rtvs, ltvs.map(IdT(_)))) }
    case _                                                    => false
  
  def subst(bodyTy: Type, tyVars: List[String], insTys: List[Type]): Type = bodyTy match
    case UnitT                          => UnitT
    case NumT                           => NumT
    case BoolT                          => BoolT
    case StrT                           => StrT
    case ArrowT(tvars, paramTys, retTy) => 
      val freeTyVars: Map[String, Type] = tyVars.zip(insTys).toMap -- tvars
      ArrowT(tvars, paramTys.map(subst(_, freeTyVars.keys.toList, insTys)), subst(retTy, freeTyVars.keys.toList, insTys))
    case IdT(name, tys)                 => 
      val substMap: Map[String, Type] = tyVars.zip(insTys).toMap
      tys match
        case Nil    => substMap.getOrElse(name, IdT(name, Nil))
        case h :: t => 
          // val bindingTyVars: List[String] = tys.map{ 
          //   case IdT(name, Nil)  => name
          //   case _               => ""}
          // val freeIdMap: Map[String, Type] = substMap -- bindingTyVars
          IdT(name, tys.map{ case t => subst(t, substMap.keys.toList, substMap.values.toList)})

  // utils
  def mustSame(lty: Type, rty: Type): Unit =
    if(lty != rty) error(s"type error: ${lty.str} != ${rty.str}")

  def sameThisType(l: Expr, r: Expr, tenv: TypeEnv, checkTy: Type, retTy: Type): Type =
    mustSame(typeCheck(l, tenv), checkTy)
    mustSame(typeCheck(r, tenv), checkTy)
    retTy
  
  def lookupVar(name: String, tenv: TypeEnv): Type =
    tenv.vars.getOrElse(name, error(s"free identifier in Ty: $name"))
  def lookupType(name: String, tenv: TypeEnv): TypeInfo =
    tenv.tys.getOrElse(name, error(s"unknown type: $name"))
  def notContainTyVar(name: String, tenv: TypeEnv): String =
    if(tenv.tys.contains(name)) error(s"already defined type: $name")
    name

  def arityCheck(l: List[_], r: List[_]): BigInt =
    if (l.length != r.length) error(s"not matching arity")
    l.length

  def errorIfF(b: Boolean, msg: String): Unit = if(!b) error(msg)

  // interpreter
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
      case ExprV(e, exEnv)  => interp(e, exEnv())
      case v                => v
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
    case ERecDefs(defs: List[RecDef], body: Expr)     =>
      lazy val finEnv: Env = defs.foldLeft(env){ case (e, d) => envUpdate(d, e, () => finEnv)}
      interp(body, finEnv)
    case EMatch(expr: Expr, mcases: List[MatchCase])  => interp(expr, env) match
      case VariantV(name, values) => mcases.find(_.name == name) match
        case Some(MatchCase(_, params, body)) =>
          arityCheck(params, values)
          interp(body, env ++ (params zip values))
        case None => error(s"no such case: $name")
      case v                      => error(s"not a variant: ${v.str}")
    case EExit(ty: Type, expr: Expr)                  => error("exit")

  def envUpdate(recDef: RecDef, curEnv: Env, updateEnv: (() => Env)): Env = recDef match
    case LazyVal(name: String, 
      ty: Type, 
      init: Expr,
    ) => curEnv + (name -> ExprV(init, updateEnv))
    case RecFun(name: String,
      tvars: List[String],
      params: List[Param],
      rty: Type,
      body: Expr,
    ) => curEnv + (name -> CloV(params.map(_.name), body, updateEnv))
    case TypeDef(
      name: String,
      tvars: List[String],
      varts: List[Variant],
    ) => 
      val vartNames: List[String] = varts.map(_.name)
      curEnv ++ vartNames.zip(vartNames.map(ConstrV(_)))

  type BOp = (BigInt, BigInt) => BigInt
  type NCOp = (BigInt, BigInt) => Boolean

  def numBOp(x: String)(op: BOp)(l: Value, r: Value): Value = (l, r) match
    case (NumV(l), NumV(r)) => NumV(op(l, r))
    case (l, r)             => error(s"invalid operation: ${l.str} $x ${r.str}")
  def numCOp(x: String)(op: NCOp)(l: Value, r: Value): Value = (l, r) match
    case (NumV(l), NumV(r)) => BoolV(op(l, r))
    case (l, r)             => error(s"invalid operation: ${l.str} $x ${r.str}")

  def lookup(x: String, env: Env): Value = env.getOrElse(x, error(s"free identifier: $x"))

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
    case _                  => error(s"invalid operation")

}