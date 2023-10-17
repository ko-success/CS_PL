package kuplrg

trait Template {

  def eval(str: String): String =
    val (v, _) = interp(Expr(str), Map.empty, Map.empty)
    v.str

  def evalCBR(str: String): String =
    val (v, _) = interpCBR(Expr(str), Map.empty, Map.empty)
    v.str

  def interp(expr: Expr, env: Env, mem: Mem): (Value, Mem)
  def interpCBR(expr: Expr, env: Env, mem: Mem): (Value, Mem)
}
