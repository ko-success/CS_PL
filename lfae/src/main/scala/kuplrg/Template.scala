package kuplrg

trait Template {

  def eval(str: String): String = interp(Expr(str), Map.empty).str
  def evalN(str: String): String = interpN(Expr(str), Map.empty).str

  def interp(expr: Expr, env: Env): Value
  def interpN(expr: Expr, env: Env): Value
}
