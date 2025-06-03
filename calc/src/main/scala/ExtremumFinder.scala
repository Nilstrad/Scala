import scala.math._
import Algebra._

object ExtremumFinder {
  def findExtremumPoints(expr: Expr, variable: String, start: Double = -100, end: Double = 100, step: Double = 0.1): List[Double] = {
    val derivative = Evaluator.differentiate(expr, variable)
    
    def findRoots(): List[Double] = {
      (BigDecimal(start) to BigDecimal(end) by BigDecimal(step)).map(_.toDouble).sliding(2).flatMap { case Seq(x1, x2) =>
        try {
          val y1 = Evaluator.evaluate(derivative, Map(variable -> x1))
          val y2 = Evaluator.evaluate(derivative, Map(variable -> x2))
          
          if (y1 * y2 < 0) {
            Some(x1 - y1 * (x2 - x1) / (y2 - y1))
          } else if (abs(y1) < 1e-6) {
            Some(x1)
          } else {
            None
          }
        } catch {
          case _: Exception => None
        }
      }.toList.distinct
    }
    
    findRoots().filter { x =>
      try {
        val y = Evaluator.evaluate(derivative, Map(variable -> x))
        abs(y) < 1e-6
      } catch {
        case _: Exception => false
      }
    }
  }
}
