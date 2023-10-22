package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  type BOp = (BigInt, BigInt) => BigInt
  def numBOp(x: String)(op: BOp)(l: Value, r: Value): Value = (l, r) match
    case (NumV(l), NumV(r)) => NumV(op(l, r))
    case (l, r) => error(s"invalid operation: ${l.str} $x ${r.str}")

  val numAdd: (Value, Value) => Value = numBOp("+")(_ + _)
  val numMul: (Value, Value) => Value = numBOp("*")(_ * _)

  def malloc(mem: Mem): Addr = mem.keySet.maxOption.fold(0)(_ + 1)
  def lookupId(env: Env, name: String): Addr = env.getOrElse(name, error(s"free identifier: $name"))

  def interp(expr: Expr, env: Env, mem: Mem): (Value, Mem) = expr match
    case Num(n)         => (NumV(n), mem)
    case Add(l, r)      =>
      val (lv, lmem) = interp(l, env, mem)
      val (rv, rmem) = interp(r, env, lmem)
      (numAdd(lv, rv), rmem)
    case Mul(l, r)      => 
      val (lv, lmem) = interp(l, env, mem)
      val (rv, rmem) = interp(r, env, lmem)
      (numMul(lv, rv), rmem)
    case Var(n, i, b)   =>
      val (iv, item) = interp(i, env, mem)
      val addr = malloc(item)
      interp(b, env + (n -> addr), item + (addr -> iv))
    case Id(x)          => (mem(lookupId(env, x)), mem)
    case Fun(p, b)      => (CloV(p, b, env), mem)
    case App(f, e)      => 
      val (fv, fmem) = interp(f, env, mem)
      fv match
        case CloV(param, body, fenv) =>
          val (av, amem) = interp(e, env, fmem)
          val addr = malloc(amem)
          interp(body, fenv + (param -> addr), amem + (addr -> av))
        case _ => error(s"not a function: ${fv.str}")
    case Assign(n, e)   =>
      val (ev, emem) = interp(e, env, mem)
      (ev, emem + (lookupId(env, n) -> ev))
    case Seq(l, r) =>
      val (_, lmem) = interp(l, env, mem)
      interp(r, env, lmem)

  def interpCBR(expr: Expr, env: Env, mem: Mem): (Value, Mem) = expr match
    case Num(n)         => (NumV(n), mem)
    case Add(l, r)      =>
      val (lv, lmem) = interpCBR(l, env, mem)
      val (rv, rmem) = interpCBR(r, env, lmem)
      (numAdd(lv, rv), rmem)
    case Mul(l, r)      => 
      val (lv, lmem) = interpCBR(l, env, mem)
      val (rv, rmem) = interpCBR(r, env, lmem)
      (numMul(lv, rv), rmem)
    case Var(n, i, b)   =>
      val (iv, item) = interpCBR(i, env, mem)
      val addr = malloc(item)
      interpCBR(b, env + (n -> addr), item + (addr -> iv))
    case Id(x)          => (mem(lookupId(env, x)), mem)
    case Fun(p, b)      => (CloV(p, b, env), mem)
    case App(fun, arg)      => 
      val (fv, fmem) = interpCBR(fun, env, mem)
      fv match
        case CloV(param, body, fenv) => arg match
          case Id(name) => 
            val addr = lookupId(env, name)
            interpCBR(body, fenv + (param -> addr), fmem)
          case _ => 
            val (av, amem) = interpCBR(arg, env, fmem)
            val addr = malloc(amem)
            interpCBR(body, fenv + (param -> addr), amem + (addr -> av))
        case _ => error(s"not a function: ${fv.str}")
    case Assign(n, e)   =>
      val (ev, emem) = interpCBR(e, env, mem)
      (ev, emem + (lookupId(env, n) -> ev))
    case Seq(l, r) =>
      val (_, lmem) = interpCBR(l, env, mem)
      interpCBR(r, env, lmem)
}
