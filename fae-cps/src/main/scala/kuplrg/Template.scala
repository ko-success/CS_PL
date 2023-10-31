package kuplrg

trait Template {

  def evalCPS(str: String): String = interpCPS(Expr(str), Map.empty, v => v).str

  def evalK(str: String): String =
    import Cont.*
    def aux(k: Cont, s: Stack): Value = reduce(k, s) match
      case (EmptyK, List(v)) => v
      case (k, s) => aux(k, s)
    aux(EvalK(Map.empty, Expr(str), EmptyK), List.empty).str

  def interpCPS(expr: Expr, env: Env, k: Value => Value): Value

  def reduce(k: Cont, s: Stack): (Cont, Stack)
}
