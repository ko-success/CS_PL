package kuplrg

trait Template {

  def eval(str: String): String = interp(Expr(str), Map.empty).str

  def evalDS(str: String): String = interpDS(Expr(str), Map.empty).str

  def interp(expr: Expr, env: Env): Value

  def interpDS(expr: Expr, env: Env): Value
}
