package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Cont.*

  def interpCPS(expr: Expr, env: Env, k: Value => Value): Value = ???

  def reduce(k: Cont, s: Stack): (Cont, Stack) = ???

}
