package kuplrg

object Implementation extends Template {

  import Expr.*
  import RecDef.*
  import Value.*
  import Type.*
  import TypeInfo.*

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
    // unit
    case EUnit                            =>
    // numbers
    case ENum(number: BigInt)             =>
    // booleans
    case EBool(bool: Boolean)             =>
    // strings
    case EStr(string: String)             =>
    // identifier lookups
    case EId(name: String)                =>
    // addition
    case EAdd(left: Expr, right: Expr)    =>
    // multiplication
    case EMul(left: Expr, right: Expr)    =>
    // division
    case EDiv(left: Expr, right: Expr)    =>
    // modulo
    case EMod(left: Expr, right: Expr)    =>
    // string concatenation
    case EConcat(left: Expr, right: Expr) =>
    // equal-to
    case EEq(left: Expr, right: Expr)     =>
    // less-than
    case ELt(left: Expr, right: Expr)     =>
    // sequence
    case ESeq(left: Expr, right: Expr)    =>
    // conditional
    case EIf(cond: Expr, thenExpr: Expr, elseExpr: Expr)
    // immutable variable definitions
    case EVal(x: String, tyOpt: Option[Type], expr: Expr, body: Expr)
    // anonymous (lambda) functions
    case EFun(params: List[Param], body: Expr)
    // function applications
    case EApp(fun: Expr, tys: List[Type], args: List[Expr])
    // mutually recursive definitions
    case ERecDefs(defs: List[RecDef], body: Expr)
    // pattern matching
    case EMatch(expr: Expr, mcases: List[MatchCase])
    // exit
    case EExit(ty: Type, expr: Expr)
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


  def interp(expr: Expr, env: Env): Value = expr match
    // unit
    case EUnit                                  => UnitV
    // numbers
    case ENum(number: BigInt)                   => NumV(number) 
    // booleans
    case EBool(bool: Boolean)                   => BoolV(bool)
    // strings
    case EStr(string: String)                   => StrV(string)
    // identifier lookups
    case EId(name: String)                      =>
      env.getOrElse(name, error(s"free identifier: $name"))
    // addition
    case EAdd(left: Expr, right: Expr)          => numAdd(interp(left, env), interp(right, env))
    // multiplication
    case EMul(left: Expr, right: Expr)          => numMul(interp(left, env), interp(right, env))
    // division
    case EDiv(left: Expr, right: Expr)          => interp(right, env) match
      case NumV(0)  => error(s"invalid operation: ${left.str} / 0")
      case rv       => numDiv(interp(left, env), rv)
    // modulo
    case EMod(left: Expr, right: Expr)          => interp(right, env) match
      case NumV(0)  => error(s"invalid operation: ${left.str} / 0")
      case rv       => numMod(interp(left, env), rv)
    // string concatenation
    case EConcat(left: Expr, right: Expr)       => interp(right, env) match
      // empty인 경우인건가?
      case StrV("") => error(s"invalid operation: ${left.str} ++ 0")
      case rv       => stringConcat(interp(left, env), rv)
    // equal-to
    case EEq(left: Expr, right: Expr)           => BoolV(eq(interp(left, env), interp(right, env)))
    // less-than
    case ELt(left: Expr, right: Expr)           => numLt(interp(left, env), interp(right, env))
    // sequence
    case ESeq(left: Expr, right: Expr)          => 
      val lv: Value = interp(left, env)
      interp(right, env)
    // conditional
    case EIf(cond: Expr, thenE: Expr, elseE: Expr)                    => interp(cond, env) match
      case BoolV(true)  => interp(thenE, env)
      case BoolV(false) => interp(elseE, env)
      case _            => error(s"not a boolean")
    // immutable variable definitions
    case EVal(x: String, tyOpt: Option[Type], expr: Expr, body: Expr) =>
      val initialValue: Value = interp(expr, env)
      interp(body, env + (x -> initialValue))
    // anonymous (lambda) functions
    case EFun(params: List[Param], body: Expr)                        => 
      val parameters: List[String] = params.map(_.name)
      CloV(parameters, body, () => env)
    // function applications
    case EApp(fun: Expr, tys: List[Type], args: List[Expr])           => 
      val argumentValues: List[Value] = args.map(e => interp(e, env))
      interp(fun, env) match
        case CloV(params: List[String], body: Expr, env: (() => Env)) =>
          val newEnv: Env = params.zip(argumentValues).foldLeft(env()){case (e, (name, value)) => e + (name -> value)}
          interp(body, newEnv)
        // 여기서 name을 id? 찾아야하나? 흠.?
        case ConstrV(name: String)                                  => VariantV(name, argumentValues)
        case _                                                      => error(s"Not a function")
    // mutually recursive definitions
    case ERecDefs(defs: List[RecDef], body: Expr)                     =>
      lazy val finEnv: Env = defs.foldLeft(env){case (curEnv, recDef) => envUpdate(recDef, curEnv, finEnv)}
      interp(body, env)// finEnv)
    // pattern matching
    case EMatch(expr: Expr, mcases: List[MatchCase])                  => interp(expr, env) match
      case VariantV(name: String, values: List[Value])  => mcases.find(_.name == name) match
        case Some(MatchCase(name, params, body))  => 
          val newEnv = params.zip(values).foldLeft(env){case (curEnv, (name, value)) => curEnv + (name -> value)}
          interp(body, newEnv)
        case _                                    => error(s"no match case")
      case _                                            => error(s"Not a variant")
    // exit
    case EExit(ty: Type, expr: Expr)                                  => error("Exit")
    // error in interpreter
    case _                                                            => error(s"error in interpreter")

  def envUpdate(recDef: RecDef, curEnv: Env, finEnv: Env): Env = recDef match
    // immutable lazy variable definition
    case LazyVal(name: String, 
      ty: Type, 
      init: Expr,
    ) => curEnv + (name -> ExprV(init, () => finEnv))
    // recursive function
    case RecFun(name: String,
      tvars: List[String],
      params: List[Param],
      rty: Type,
      body: Expr,
    ) => curEnv + (name -> (CloV(params.map(_.name), body, () => finEnv)))
    // polymorphic algebraic data type
    case TypeDef(
      name: String,
      tvars: List[String],
      varts: List[Variant],
    ) => varts.map(v => ConstrV(v.name)).foldLeft(curEnv){case (e, ConstrV(name)) => e + (name -> ConstrV(name))}

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
  val numLt: (Value, Value) => Value = numCOp("<")(_ < _)

  // 수정필요
  def eq(l: Value, r: Value): Boolean = (l, r) match
    case (UnitV, UnitV)         => true
    case (NumV(l), NumV(r))     => l == r
    case (BoolV(l), BoolV(r))   => l == r
    case (StrV(l), StrV(r))     => l == r
    case (VariantV(lname, llist), VariantV(rname, rrist)) => 
      llist.zip(rrist).foldLeft(lname == rname){case (i, (l, r)) => i && eq(l, r)}
    case _                      => false

  def stringConcat(l: Value, r: Value): StrV = (l, r) match
    case (StrV(l), StrV(r)) => StrV(l + r)
    case _  => error(s"invalid operation")
}
