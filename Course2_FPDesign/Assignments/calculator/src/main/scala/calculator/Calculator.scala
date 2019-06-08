package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map(ne => (ne._1, Signal(eval(ne._2(), namedExpressions))))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def transverseExpr(expr: Expr, history: List[String]): Double = {
      expr match {
        case Literal(v) => v
        case Ref(name) => if(references.contains(name) && !history.contains(name))
                            transverseExpr(references(name)(), name :: history)
                          else
                            Double.NaN
        case Plus(a, b) => transverseExpr(a, history) + transverseExpr(b, history)
        case Minus(a, b) => transverseExpr(a, history) - transverseExpr(b, history)
        case Times(a, b) => transverseExpr(a, history) * transverseExpr(b, history)
        case Divide(a, b) => transverseExpr(a, history) / transverseExpr(b, history)
      }
    }
    transverseExpr(expr, List())
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
