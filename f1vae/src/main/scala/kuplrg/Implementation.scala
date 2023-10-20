package kuplrg

object Implementation extends Template {

  import Expr.*

  def interp(expr: Expr, env: Env, fenv: FEnv): Value = expr match
    case Num(n)         => n
    case Add(l, r)      => interp(l, env, fenv) + interp(r, env, fenv)
    case Mul(l, r)      => interp(l, env, fenv) * interp(r, env, fenv)
    case Val(x, e, b)   => interp(b, env + (x -> interp(e, env, fenv)), fenv)
    case Id(x)          => env.getOrElse(x, error(s"free identifier: $x"))
    case App(f, e)      => 
      val fdef = fenv.getOrElse(f, error(s"unknown function: $f"))
      interp(fdef.body, Map(fdef.param -> interp(e, env, fenv)), fenv)

  def interpDS(expr: Expr, env: Env, fenv: FEnv): Value = expr match
    case Num(n)         => n
    case Add(l, r)      => interpDS(l, env, fenv) + interpDS(r, env, fenv)
    case Mul(l, r)      => interpDS(l, env, fenv) * interpDS(r, env, fenv)
    case Val(x, e, b)   => interpDS(b, env + (x -> interpDS(e, env, fenv)), fenv)
    case Id(x)          => env.getOrElse(x, error(s"free identifier: $x"))
    case App(f, e)      => 
      val fdef = fenv.getOrElse(f, error(s"unknown function: $f"))
      interpDS(fdef.body, env + (fdef.param -> interpDS(e, env, fenv)), fenv)
}
