package kuplrg

object Implementation extends Template {

  import Expr.*
  import RecDef.*
  import Value.*
  import Type.*
  import TypeInfo.*

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
    // unit
    case EUnit                                        =>
    // numbers
    case ENum(number: BigInt)                         =>
    // booleans
    case EBool(bool: Boolean)                         =>
    // strings
    case EStr(string: String)                         =>
    // identifier lookups
    case EId(name: String)                            =>
    // addition
    case EAdd(left: Expr, right: Expr)                =>
    // multiplication
    case EMul(left: Expr, right: Expr)                =>
    // division
    case EDiv(left: Expr, right: Expr)                =>
    // modulo
    case EMod(left: Expr, right: Expr)                =>  
    // string concatenation
    case EConcat(left: Expr, right: Expr)             =>
    // equal-to
    case EEq(left: Expr, right: Expr)                 =>
    // less-than
    case ELt(left: Expr, right: Expr)                 =>
    // sequence
    case ESeq(left: Expr, right: Expr)                =>
    // conditional
    case EIf(cond: Expr, 
      thenExpr: Expr, 
      elseExpr: Expr)                                 =>
    // immutable variable definitions
    case EVal(x: String, 
      tyOpt: Option[Type], 
      expr: Expr, body: Expr)                         =>
    // anonymous (lambda) functions
    case EFun(params: List[Param], body: Expr)        =>
    // function applications
    case EApp(fun: Expr, 
      tys: List[Type], args: List[Expr])              =>
    // mutually recursive definitions
    case ERecDefs(defs: List[RecDef], body: Expr)     =>
    // pattern matching
    case EMatch(expr: Expr, mcases: List[MatchCase])  =>
    // exit
    case EExit(ty: Type, expr: Expr)                  =>
    // error in type checking
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
      case BoolV(true)  => interp(thenE, env)
      case BoolV(false) => interp(elseE, env)
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
      lazy val finEnv: Env = defs.foldLeft(env){ case (cur, upd) => envUpdate()}
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

  def lookup(x: String, env: Env): Value = env.getOrElse(x, error(s"free identifer: $name"))

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
