package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Inst.*
  import Control.*

  // ---------------------------------------------------------------------------
  // Problem #1
  // ---------------------------------------------------------------------------
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

  def reduce(st: State): State = st match
    // IEval
    case State(IEval(env, expr) :: cont, stack, handler, memory)              => expr match
      // base Eval
      case EUndef                     => State(cont, UndefV :: stack, handler, memory)
      case ENum(num)                  => State(cont, NumV(num) :: stack, handler, memory)
      case EBool(bool)                => State(cont, BoolV(bool) :: stack, handler, memory)
      case EAdd(left, right)          => State(IEval(env, left) :: IEval(env, right) :: IAdd :: cont, stack, handler, memory)
      case EMul(left, right)          => State(IEval(env, left) :: IEval(env, right) :: IMul :: cont, stack, handler, memory)
      case EDiv(left, right)          => State(IEval(env, left) :: IEval(env, right) :: IDiv :: cont, stack, handler, memory)
      case EMod(left, right)          => State(IEval(env, left) :: IEval(env, right) :: IMod :: cont, stack, handler, memory)
      case EEq(left, right)           => State(IEval(env, left) :: IEval(env, right) :: IEq :: cont, stack, handler, memory)
      case ELt(left, right)           => State(IEval(env, left) :: IEval(env, right) :: ILt :: cont, stack, handler, memory)
      case EVar(name, init, body)     => State(IEval(env, init) :: IDef(List(name), env, body) :: cont, stack, handler, memory)
      case EId(name)                  => State(cont, lookup(memory, lookup(env, name)) :: stack, handler, memory)
      case EAssign(name, expr)        => State(IEval(env, expr) :: IWrite(lookup(env, name)) :: cont, stack, handler, memory)
      case ESeq(left, right)          => State(IEval(env, left) :: IPop :: IEval(env, right) :: cont, stack, handler, memory)

      // condtional & loop Eval
      case EIf(cond, thenE, elseE)    => 
        val ifCont:KValue = KValue(IEval(env, thenE) :: cont, stack, handler)
        State(IEval(env, cond) :: IJmpIf(ifCont) :: IEval(env, elseE) :: cont, stack, handler, memory)
      case EWhile(cond, body)         => 
        val breakCont: KValue = KValue(cont, stack, handler)
        val continueCont: KValue = KValue(IPop :: IEval(env, EWhile(cond, body)) :: cont, stack, handler)
        val whileHandler: Handler = handler + (Continue -> continueCont) + (Break -> breakCont) // 연속으로 추가하는게 가능한건가?
        val whileCont: KValue = KValue(IEval(env, body) :: IJmp(Continue) :: Nil, stack, whileHandler)
        State(IEval(env, cond) :: IJmpIf(whileCont) :: cont, UndefV :: stack, handler, memory)
      case EBreak                     => State(IJmp(Break) :: Nil, UndefV :: stack, handler, memory)
      case EContinue                  => State(IJmp(Continue) :: Nil, UndefV :: stack, handler, memory)

      // functions & return Eval
      case EFun(params, body)         => State(cont, CloV(params, body, env) :: stack, handler, memory)
      case EApp(fun, args)            => 
        val argsList: List[Inst] = args.map(IEval(env, _))
        State((IEval(env, fun) :: argsList) ::: (ICall(argsList.length) :: cont), stack, handler, memory)
      case EReturn(expr)              => State(IEval(env, expr) :: IReturn :: cont, stack, handler, memory)

      // exception Eval
      case ETry(body, catchP, catchE) => 
        val finallyCont: KValue = KValue(cont, stack, handler)
        val throwCont: KValue = KValue(IDef(List(catchP), env, catchE) :: cont, stack, handler)
        val tryHandler: Handler = handler + (Throw -> throwCont) + (Finally -> finallyCont) // MAP(Throw -> throwCont, Finally -> finallyCont) 인건가ㅏ?
        State(IEval(env, body) :: IJmp(Finally) :: Nil, stack, tryHandler, memory)
      case EThrow(expr)               => State(IEval(env, expr) :: IJmp(Throw) :: Nil, stack, handler, memory)

      // generator Eval
      case EGen(params, body)         => State(cont, GenV(params, body, env) :: stack, handler, memory)
      case EIterNext(iter, arg)       => arg match
        case None       => State(IEval(env, iter) :: IEval(env, EUndef) :: INext :: cont, stack, handler, memory)
        case Some(expr) => State(IEval(env, iter) :: IEval(env, expr) :: INext :: cont, stack, handler, memory)
      case EYield(expr)               => 
        val nextCont: KValue = KValue(cont, stack, handler)
        State(IEval(env, expr) :: IYield :: Nil, BoolV(false) :: ContV(nextCont) :: stack, handler, memory)
      case EValueField(result)        => State(IEval(env, result) :: IValueField :: cont, stack, handler, memory)
      case EDoneField(result)         => State(IEval(env, result) :: IDoneField :: cont, stack, handler, memory)


    // second Eval instructions
    case State(IAdd :: cont, n2 :: n1 :: stack, handler, memory)              => State(cont, numAdd(n1, n2) :: stack, handler, memory)
    case State(IMul :: cont, n2 :: n1 :: stack, handler, memory)              => State(cont, numMul(n1, n2) :: stack, handler, memory)
    case State(IDiv :: cont, n2 :: n1 :: stack, handler, memory)              => n2 match
      case NumV(0)  => error(s"invalid operation: ${n2.str} % 0")
      case v        => State(cont, numDiv(n1, n2) :: stack, handler, memory)
    case State(IMod :: cont, n2 :: n1 :: stack, handler, memory)              => n2 match
      case NumV(0)  => error(s"invalid operation: ${n2.str} % 0")
      case v        => State(cont, numMod(n1, n2) :: stack, handler, memory)
    case State(IEq :: cont, v2 :: v1 :: stack, handler, memory)               => State(cont, BoolV(eq(v1, v2)) :: stack, handler, memory)
    case State(ILt :: cont, n2 :: n1 :: stack, handler, memory)               => State(cont, numLt(n1, n2) :: stack, handler, memory)
    
    // second def & stack instructions
    case State(IDef(xs, env, body) :: cont, stack, handler, memory)           => 
      if(stack.length >= xs.length)
        val valList: Stack = stack.take(xs.length)
        val restStack: Stack = stack.drop(xs.length)
        val addrList: List[Addr] = malloc(memory, xs.length)
        val newEnv: Env = env ++ xs.zip(addrList).toMap
        val newMemory: Mem = memory ++ addrList.zip(valList.reverse).toMap
        State(IEval(newEnv, body) :: cont, restStack, handler, newMemory)
      else error()
    case State(IWrite(addr) :: cont, v :: stack, handler, memory)             => State(cont, v :: stack, handler, memory + (addr -> v))
    case State(IPop :: cont, v :: stack, handler, memory)                     => State(cont, stack, handler, memory)

    // control flow instructions
    case State(IJmpIf(kv) :: cont , condV :: stack, handler, memory)          => condV match
      case BoolV(true)  => 
        val KValue(jmpCont, jmpStack, jmpHandler) = kv
        State(jmpCont, jmpStack, jmpHandler, memory)
      case BoolV(false) => State(cont, stack, handler, memory) // false일 경우 IJmpIf(kv)의 kv가 뭔지 계산도 안함에 유의
      case _            => error() // boolean error
    case State(IJmp(control) :: cont, v :: stack, handler, memory)                 => 
      val KValue(jmpCont, jmpStack, jmpHandler) = lookup(handler, control)
      val yieldHandler: Handler = handler.getOrElse(Yield, Nil) match // 이게 되는건가?
        case Nil        => jmpHandler
        case kv: KValue => jmpHandler + (Yield -> kv)
      State(jmpCont, v :: jmpStack, yieldHandler, memory)

    // functionc call & return instructions
    case State(ICall(argSize) :: cont, stack, handler, memory)                => 
      if(stack.length > argSize) // 이거가 필요할까?
        val valList: Stack = stack.take(argSize)
        val (callV :: restStack) = stack.drop(argSize)
        callV match
          case CloV(params, body, fenv) =>
            val returnCont: KValue = KValue(cont, restStack, handler)
            val callHandler: Handler = handler + (Return -> returnCont) - Break - Continue - Yield
            val callStack: Stack = 
              if(argSize >= params.length)
                valList.slice(argSize - params.length, argSize) 
              else List.fill(params.length - argSize)(UndefV) ::: valList
            State(IDef(params, fenv, body) :: Nil, callStack, callHandler, memory)
          case GenV(params, body, fenv) =>
            val addr: Addr = malloc(memory)
            val genStack: Stack = 
              if(argSize >= params.length)
                valList.slice(argSize - params.length, argSize) 
              else List.fill(params.length - argSize)(UndefV) ::: valList
            val genCont: KValue = KValue(IPop :: IDef(params, fenv, EReturn(ETry(body, "anyId", EId("anyId")))) :: Nil, genStack, Map()) // semantic이 이게 맞나?
            State(cont, IterV(addr) :: restStack, handler, memory + (addr -> ContV(genCont)))
          case _                        => error()
      else error() 
    case State(IReturn :: cont, v :: stack, handler, memory)                  => 
      handler.getOrElse(Yield, Nil) match
        case Nil  => State(IJmp(Return) :: Nil, v :: Nil, handler, memory)
        case kv   => 
          val doneCont: KValue = KValue(IReturn :: Nil, Nil, Map())
          State(IYield :: Nil, v :: BoolV(true) :: ContV(doneCont) :: stack, handler, memory)
    case State(INext :: cont, v :: IterV(a) :: stack, handler, memory)        =>
      val nextCont: KValue = KValue(cont, IterV(a) :: stack, handler)
      val ContV(KValue(aCont, aStack, aHandler)) = lookup(memory, a)
      val nextHandler: Handler = handler + (Yield -> nextCont) + (Return -> nextCont)
      State(aCont, v :: aStack, nextHandler, memory)
    case State(IYield :: _, v1 :: BoolV(b) :: v2 :: _, handler, memory)       => // boolV(b)가 필요한가?
      val KValue(yCont, IterV(a) :: yStack, yHandler) = lookup(handler, Yield) // IterV(a)인지도 check해야하는가?
      State(yCont, ResultV(v1, b) :: yStack, yHandler, memory + (a -> v2))
    case State(IValueField :: cont, ResultV(v, _) :: stack, handler, memory)  => State(cont, v :: stack, handler, memory)
    case State(IDoneField :: cont, ResultV(_, b) :: stack, handler, memory)   => State(cont, BoolV(b) :: stack, handler, memory) // b가 Boolv인지 check해야하나?

  // ---------------------------------------------------------------------------
  // Problem #2
  // ---------------------------------------------------------------------------
  def bodyOfSquares: String = "not implement"

  // ---------------------------------------------------------------------------
  // Helper functions
  // ---------------------------------------------------------------------------
  def malloc(mem: Mem, n: Int): List[Addr] =
    val a = malloc(mem)
    (0 until n).toList.map(a + _)

  def malloc(mem: Mem): Addr = mem.keySet.maxOption.fold(0)(_ + 1)

  def lookup(mem: Mem, x: Addr): Value =
    mem.getOrElse(x, error(s"address is not allocated: $x"))

  def lookup(env: Env, x: String): Addr =
    env.getOrElse(x, error(s"free identifier: $x"))

  def lookup(handler: Handler, x: Control): KValue =
    handler.getOrElse(x, error(s"invalid control operation: $x"))

  def eq(l: Value, r: Value): Boolean = (l, r) match
    case (UndefV, UndefV) => true
    case (NumV(l), NumV(r)) => l == r
    case (BoolV(l), BoolV(r)) => l == r
    case (IterV(l), IterV(r)) => l == r
    case (ResultV(lv, ld), ResultV(rv, rd)) => eq(lv, rv) && ld == rd
    case _ => false
}
