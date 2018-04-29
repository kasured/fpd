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
    namedExpressions.mapValues(signalExpr => Signal(eval(signalExpr(), namedExpressions)(Set())))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]])(seen: Set[String]): Double = expr match {
    case Literal(value) => value
    case Plus(a, b) => eval(a, references)(seen) + eval(b,references)(seen)
    case Minus(a, b) => eval(a, references)(seen) - eval(b,references)(seen)
    case Times(a, b) => eval(a, references)(seen) * eval(b,references)(seen)
    case Divide(a, b) => eval(a, references)(seen) / eval(b,references)(seen)
    case Ref(name) if seen contains name => Double.NaN
    case Ref(name) => eval(getReferenceExpr(name, references), references)(seen + name)
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
