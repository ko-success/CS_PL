package kuplrg

object Implementation extends Template {

  import Expr.*

  def interp(expr: Expr, env: Env): Value = ???

  def freeIds(expr: Expr): Set[String] = ???

  def bindingIds(expr: Expr): Set[String] = ???

  def boundIds(expr: Expr): Set[String] = ???

  def shadowedIds(expr: Expr): Set[String] = ???

}
