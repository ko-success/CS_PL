package kuplrg

trait Template {

  def eval(str: String): String = interp(Expr(str)).toString

  def interp(expr: Expr): Value

  def countNums(expr: Expr): Int
}
