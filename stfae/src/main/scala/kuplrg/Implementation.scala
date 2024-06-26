package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def mustSame(lty: Type, rty: Type): Unit = 
    if(lty != rty) error(s"type error: ${lty.str} != ${rty.str}")
  
  def lookup(x: String, tenv: TypeEnv) = tenv.getOrElse(x, error(s"free identifier: $x"))
  
  def errorIfF(b: Boolean) = if(!b) error("type error")
  
  def subtypeRelation(lty: Type, rty: Type): Boolean = (lty, rty) match
    case (BotT, _)  => true
    case (_, TopT)  => true
    case (ArrowT(lpty, lrty), ArrowT(rpty, rrty)) => 
      errorIfF(subtypeRelation(rpty, lpty))
      errorIfF(subtypeRelation(lrty, rrty))
      true
    // RecordT(fields: Map[String, Type])
    case (RecordT(lfields), RecordT(rfields)) => 
      val rule1: Boolean = rfields.keySet.subsetOf(lfields.keySet)
      // rule 3(permutation)은 sort로 해결
      val lfiltered: Map[String, Type] = lfields.filterKeys(rfields.contains).toMap
      val lsorted: List[(String, Type)] = lfiltered.toSeq.sortBy(_._1).toList
      val rsorted: List[(String, Type)] = rfields.toSeq.sortBy(_._1).toList
      lazy val rule2: Boolean = lsorted.zip(rsorted).foldLeft(true){ case (b, (lf, rf)) => b && (lf._1 == rf._1) && (subtypeRelation(lf._2, rf._2))}
      rule1 && rule2
    case (l, r) => l == r
    case t            => error(s"subtype relation error")

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
    case Num(number: BigInt)                      => NumT
    case Add(left: Expr, right: Expr)             => 
      errorIfF(subtypeRelation(typeCheck(left, tenv), NumT))
      errorIfF(subtypeRelation(typeCheck(right, tenv), NumT))
      NumT
    case Mul(left: Expr, right: Expr)             =>
      errorIfF(subtypeRelation(typeCheck(left, tenv), NumT))
      errorIfF(subtypeRelation(typeCheck(right, tenv), NumT))
      NumT
    case Val(name: String,
      tyOpt: Option[Type], 
      init: Expr, body: Expr)                     =>
      val initTy = typeCheck(init, tenv)
      tyOpt.map(givenTy => errorIfF(subtypeRelation(initTy, givenTy)))
      val nameTy = tyOpt.getOrElse(initTy)
      typeCheck(body, tenv + (name -> nameTy))
    case Id(name: String)                         => lookup(name, tenv)
    case Fun(param: String, ty: Type, body: Expr) => 
      val retTy = typeCheck(body, tenv + (param -> ty))
      ArrowT(ty, retTy)
    case App(fun: Expr, arg: Expr)                => typeCheck(fun, tenv) match
      case ArrowT(paramTy, retTy) => 
        val argTy = typeCheck(arg, tenv)
        errorIfF(subtypeRelation(argTy, paramTy))
        retTy
      case t                      => error(s"not a function type: ${t.str}")
    case Record(fields: List[(String, Expr)])     => 
      RecordT(fields.map{ case (f, e) => (f, typeCheck(e, tenv)) }.toMap)
    case Access(record: Expr, field: String)      => typeCheck(record, tenv) match
      case RecordT(fields)  => fields.getOrElse(field, error(s"no such field: $field"))
      case t                => error(s"not a record type: ${t.str}")
    case Exit                                     => BotT

  type BOp = (BigInt, BigInt) => BigInt

  def numBOp(x: String)(op: BOp)(l: Value, r: Value): Value = (l, r) match
    case (NumV(l), NumV(r)) => NumV(op(l, r))
    case (l, r) => error(s"invalid operation: ${l.str} $x ${r.str}")

  def lookup(x: String, env: Env): Value = env.getOrElse(x, error(s"free identifier: $x"))

  val numAdd: (Value, Value) => Value = numBOp("+")(_ + _)
  val numMul: (Value, Value) => Value = numBOp("*")(_ * _)


  def interp(expr: Expr, env: Env): Value = expr match
    case Num(number: BigInt)                      => NumV(number)
    case Add(left: Expr, right: Expr)             => numAdd(interp(left, env), interp(right, env))
    case Mul(left: Expr, right: Expr)             => numMul(interp(left, env), interp(right, env))
    case Val(name: String,
      tyOpt: Option[Type], 
      init: Expr, body: Expr)                     => interp(body, env + (name -> interp(init, env)))
    case Id(name: String)                         => lookup(name, env)
    case Fun(param: String, ty: Type, body: Expr) => CloV(param, body, env)
    case App(fun: Expr, arg: Expr)                => interp(fun, env) match
      case CloV(p, b, fenv) => interp(b, fenv + (p -> interp(arg, env)))
      case v                => error(s"not a function: ${v.str}")
    case Record(fields: List[(String, Expr)])     => 
      RecordV(fields.map{ case (field, expr) => (field, interp(expr, env)) }.toMap)
    case Access(record: Expr, field: String)      => interp(record, env) match
      case RecordV(fields)  => fields.getOrElse(field, error(s"no such field: $field"))
      case v                => error(s"not a record: ${v.str}")
    case Exit                                     => error("exit")
}
