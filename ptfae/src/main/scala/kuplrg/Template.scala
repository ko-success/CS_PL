package kuplrg

trait Template {

  def eval(str: String): String =
    val expr = Expr(str)
    val ty = typeCheck(expr, TypeEnv())
    val v = interp(expr, Map.empty)
    s"${v.str}: ${ty.str}"

  def typeCheck(expr: Expr, tenv: TypeEnv): Type

  def interp(expr: Expr, env: Env): Value

}
