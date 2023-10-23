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
    case Id(x)          => (env.getOrElse(x, error(s"free identifier: $x")), mem)
    case Fun(p, b)      => (CloV(p, b, env), mem)
    case App(f, e)      => interp(f, env, mem) match
      case (CloV(p, b, fenv), fmem) => 
        val (ini, pmem) = interp(e, env, fmem)
        interp(b, fenv + (p -> ini), pmem)
      case (v, _) => error(s"not a function: ${v.str}")
    case NewBox(c) =>
      val (cv, cmem) = interp(c, env, mem)
      val addr = malloc(cmem)
      (BoxV(addr), cmem + (addr -> cv))
    case GetBox(b) =>
      val (bv, bmem) = interp(b, env, mem)
      bv match
        case BoxV(addr) => (bmem(addr), bmem)
        case _ => error(s"not a box: ${bv.str}")
    case SetBox(b, c) => 
      val (bv, bmem) = interp(b, env, mem)
      bv match
        case BoxV(addr) =>
          val (cv, cmem) = interp(c, env, bmem)
          (cv, cmem + (addr -> cv))
        case _ =>
          error(s"not a box: ${bv.str}")
    case Seq(l, r) =>
      val (_, lmem) = interp(l, env, mem)
      interp(r, env, lmem)
}
