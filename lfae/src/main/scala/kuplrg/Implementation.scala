package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  def strict(v: Value): Value = v match
    case ExprV(e, env) => strict(interp(e, env))
    case _ => v

  type BOp = (BigInt, BigInt) => BigInt
  def numBOp(x: String)(op: BOp)(l: Value, r: Value): Value = (strict(l), strict(r)) match
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
    case App(f, e)      => strict(interp(f, env)) match
      case CloV(p, b, fenv) => interp(b, fenv + (p -> ExprV(e, env)))
      case v => error(s"not a function: ${v.str}")

  def strictN(v: Value): Value = v match
    case ev @ ExprVN(e, env, c) => c match
      case Some(cache) => cache
      case None => 
        val cache = strictN(interpN(e, env)) // 왜 얘는 이렇게 안하지????
        ExprVN(e, env, Some(cache)) // Expr을 반환하면 상위에서 env 업뎃해주고 내용 가져가는식
        // val cache = interp(e, env)
        // ev.value = Some(cache)
        // cache
    case _ => v

  def numBOpN(x: String)(op: BOp)(l: Value, r: Value): Value = (l, r) match
    case (NumV(l), NumV(r)) => NumV(op(l, r))
    case (l, r) => error(s"invalid operation: ${l.str} $x ${r.str}")

  val numAddN: (Value, Value) => Value = numBOpN("+")(_ + _)
  val numMulN: (Value, Value) => Value = numBOpN("*")(_ * _)

  def interpN(expr: Expr, env: Env): Value = expr match
    case Num(n)         => NumV(n)
    case Add(l, r)      => strictN(interpN(l, env)) match 
      case ExprVN(e, lenv, Some(c)) =>  // l의 결과가 처음 계산한 exprV이면 r의 env의 exprV을 바꿔줘야함
        val sl = c
        val cachedExpr = ExprVN(e, lenv, Some(c))
        val i = env.find { case (key, value) => value == cachedExpr } // env안에서 참조한 exprV의 id를 역으로 find하는 과정
        val idOfExpr = i match
          case Some((key, _)) => key
          case None => ""
        val sr = strictN(interpN(r, lenv + (idOfExpr -> cachedExpr))) 
        numAddN(sl, sr)
    case Mul(l, r)      => strictN(interpN(l, env)) match 
      case ExprVN(e, lenv, Some(c)) =>
        val sl = c
        val cachedExpr = ExprVN(e, lenv, Some(c))
        val i = env.find { case (key, value) => value == cachedExpr }
        val idOfExpr = i match
          case Some((key, _)) => key
          case None => ""
        val sr = strictN(interpN(r, lenv + (idOfExpr -> cachedExpr))) 
        numMulN(sl, sr)
    case Id(x)          => env.getOrElse(x, error(s"free identifier: $x"))
    case Fun(p, b)      => CloV(p, b, env)
    case App(f, e)      =>  (interpN(f, env)) match
      case ExprVN(e, env, Some(c)) => c match
        case CloV(p, b, fenv) => interpN(b, fenv + (p -> ExprVN(e, env, None)))
      case CloV(p, b, fenv) => interpN(b, fenv + (p -> ExprVN(e, env, None)))
      case v => error(s"not a function: ${v.str}")

}