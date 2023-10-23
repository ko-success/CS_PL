package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

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

  def interp(expr: Expr, env: Env): Value = expr match
    case EUnit              => UnitV
    case ENum(n)            => NumV(n)
    case EBool(b)           => BoolV(b)
    case EId(n)             => env.getOrElse(n, error(s"free identifier: $n"))
    case EAdd(l, r)         => numAdd(interp(l, env), interp(r, env))
    case EMul(l, r)         => numMul(interp(l, env), interp(r, env))
    case EDiv(l, r)         => interp(r, env) match
      case NumV(0)  => error(s"invalid operation: ${l.str} / 0")
      case rv       => numDiv(interp(l, env), rv)
    case EMod(l, r)         => interp(r, env) match
      case NumV(0)  => error(s"invalid operation: ${l.str} % 0")
      case rv       => numMod(interp(l, env), rv)
    case EEq(l, r)          => eq(interp(l, env), interp(r,env)) match
      case true  => BoolV(true)
      case false => BoolV(false)
    case ELt(l, r)          => numLt(interp(l, env), interp(r,env))
    case EIf(c, t, e)       => interp(c, env) match
      case BoolV(true)  => interp(t, env)
      case BoolV(false) => interp(e, env)
      case v            => error(s"not a boolean: ${v.str}")
    case ENil               => NilV
    case ECons(h, t)        => (interp(h, env), interp(t, env)) match
      case (NilV, tv) => error(s"invalid operation: ${NilV.str} :: ${tv.str}")
      case (hv, ConsV(ht, tt))  => ConsV(hv, ConsV(ht, tt))
      case (hv, NilV)           => ConsV(hv, NilV)
    case EHead(l)           => interp(l, env) match
      case ConsV(h, t)  => h
      case NilV         => error(s"empty list: ${NilV.str}")
      case v            => error(s"not a list: ${v.str}")
    case ETail(l)           => interp(l, env) match
      case ConsV(h, t)  => t
      case NilV         => error(s"empty list: ${NilV.str}")
      case v            => error(s"not a list: ${v.str}")
    case ELength(l)         => NumV(length(interp(l, env)))
    case EMap(l, f)         => interp(f, env) match
      case CloV(p, b, e)  => interp(l, env) match
        case ConsV(h, t)  => map(ConsV(h, t), CloV(p, b, e))
        case NilV         => map(NilV, CloV(p, b, e))
        case v            => error(s"not a list: ${v.str}")
      case v                  => error(s"not a function: ${v.str}")
    case EFlatMap(l, f)     => interp(f, env) match
      case CloV(p, b, e)  => interp(l, env) match
        case ConsV(h, t)  => join(map(ConsV(h, t), CloV(p, b, e)))
        case NilV         => join(map(NilV, CloV(p, b, e)))
        case v            => error(s"not a list: ${v.str}")
      case v                  => error(s"not a function: ${v.str}")
    case EFilter(l, f)      =>interp(f, env) match
      case CloV(p, b, e)  => interp(l, env) match
        case ConsV(h, t)  => filter(ConsV(h, t), CloV(p, b, e))
        case NilV         => filter(NilV, CloV(p, b, e))
        case v            => error(s"not a list: ${v.str}")
      case v                  => error(s"not a function: ${v.str}")
    case ETuple(es)         => es.length match
      case 0  => UnitV
      case _  => TupleV(es.map(e => interp(e, env)))
    case EProj(t, i)        => interp(t, env) match
      case TupleV(vs) =>
        if(vs.length < i) error(s"out of bounds: ${t.str}(${i})")
        else vs(i-1)
      case _          => error(s"not a tuple: ${t.str}")
    case EVal(n, v, s)      => interp(s, env + (n -> interp(v, env)))
    case EFun(ps, b)        => CloV(ps, b, () => env)
  // mutually recursive function
    case ERec(ds, s)        => 
      lazy val renv: Env = ds.foldLeft(env){case (e, FunDef(n, ps, b)) => e + (n -> CloV(ps, b, (() => renv)))}
      interp(s, renv)
    case EApp(f, as)        => interp(f, env) match
      case CloV(p, b, fenv)   => app(CloV(p, b, fenv), as.map(e => interp(e, env)))
      case v                  => error(s"not a function: ${v.str}")

  def eq(left: Value, right: Value): Boolean = (left, right) match
    case (UnitV, UnitV)                 => true
    case (NumV(l), NumV(r))             => (l == r)
    case (BoolV(l), BoolV(r))           => (l == r)
    case (NilV, NilV)                   => true
    case (ConsV(lh, lt), ConsV(rh, rt)) => eq(lh, rh) && eq(lt, rt)
    case (NilV, ConsV(_, _))            => false
    case (ConsV(_, _), NilV)            => false
    case (TupleV(lv), TupleV(rv))       => 
      if(lv.length != rv.length) false 
      else lv.zip(rv).foldLeft(true){case (x, (l, r)) => x && eq(l, r)}
    case (l, r)                         => 
      error(s"invalid operation: ${l.str} == ${r.str}")

  def length(list: Value): BigInt = list match
      case ConsV(_, t)  => 1 + length(t)
      case NilV         => 0
      case _            => error(s"not a list: ${list.str}")

  def map(list: Value, fun: Value): Value = (list, fun) match
    case (ConsV(h, t), f) => ConsV(app(f, List(h)), map(t, f))
    case (NilV, _)                    => NilV
    case (_, _)                       =>
      error(s"invalid operation: ${fun.str}(${list.str})")

  def join(list: Value): Value = list match
    case ConsV(ConsV(hh, th), t)  => ConsV(hh, join(ConsV(th, t)))
    case ConsV(NilV, t)           => join(t)
    case NilV                     => NilV

  def filter(list: Value, fun: Value): Value = (list, fun) match
    case (ConsV(h, t), f) => app(f, List(h)) match
      case BoolV(true)  => ConsV(h, filter(t, f))
      case BoolV(false) => filter(t, f)
      case v            => error(s"not a boolean: ${v.str}")
    case (NilV, _)                    => NilV
    case (_, _)                       =>
      error(s"invalid operation: ${fun.str}(${list.str})")

  def app(fun: Value, args: List[Value]): Value = fun match
    case CloV(p, b, env)  =>
      if(p.length > args.length)
        val fenv = p.zip(args ++ List.fill(p.length - args.length)(UnitV)).foldLeft(env()){case (m, (k, v)) => m + (k -> v)}
        interp(b, fenv)
      else
        val fenv = p.zip(args.take(p.length)).foldLeft(env()){case (m, (k, v)) => m + (k -> v)}
        interp(b, fenv)
}
