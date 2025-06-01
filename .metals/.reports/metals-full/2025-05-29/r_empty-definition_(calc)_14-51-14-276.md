error id: file:///C:/Users/artam/Desktop/Scala%202.0/Scala/calc/src/main/scala/ExtremeFinder.scala:`<none>`.
file:///C:/Users/artam/Desktop/Scala%202.0/Scala/calc/src/main/scala/ExtremeFinder.scala
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -scala/math/Differentiator.
	 -Differentiator.
	 -scala/Predef.Differentiator.
offset: 727
uri: file:///C:/Users/artam/Desktop/Scala%202.0/Scala/calc/src/main/scala/ExtremeFinder.scala
text:
```scala
import scala.annotation.tailrec
import scala.math._

object ExtremaFinder {
  type Function = Double => Double
  type Solver = (Function, Double, Double, Double) => Either[ExtremaError, List[Double]]

  sealed trait ExtremaError
  case class NoConvergence(maxIterations: Int) extends ExtremaError
  case class InvalidInterval(a: Double, b: Double) extends ExtremaError
  case class EvaluationError(details: String) extends ExtremaError

  // Основной интерфейс
  def findExtrema(
    expr: Expr, 
    variable: String,
    searchInterval: (Double, Double) = (-10.0, 10.0),
    step: Double = 0.1,
    tolerance: Double = 1e-6,
    maxIterations: Int = 100
  ): Either[ExtremaError, List[Double]] = {
    val derivative = Differ@@entiator.differentiate(expr, variable)
    val derivativeFn = buildFunction(derivative, variable)
    
    for {
      _ <- validateInterval(searchInterval)
      candidates <- findRootCandidates(derivativeFn, searchInterval, step)
      roots <- refineRoots(derivativeFn, candidates, tolerance, maxIterations)
    } yield roots
  }

  // Валидация интервала поиска
  private def validateInterval(interval: (Double, Double)): Either[InvalidInterval, Unit] = {
    if (interval._1 >= interval._2) Left(InvalidInterval(interval._1, interval._2))
    else Right(())
  }

  // Построение функции из выражения
  private def buildFunction(expr: Expr, variable: String): Function = {
    x => Evaluator.evaluate(expr, Map(variable -> x))
  }

  // Поиск кандидатов в корни (где функция меняет знак)
  private def findRootCandidates(
    fn: Function,
    interval: (Double, Double),
    step: Double
  ): Either[ExtremaError, List[(Double, Double)]] = {
    @tailrec
    def loop(x: Double, acc: List[(Double, Double)]): List[(Double, Double)] = {
      if (x >= interval._2) acc.reverse
      else {
        val nextX = x + step
        if (nextX > interval._2) acc.reverse
        else {
          val y1 = fn(x)
          val y2 = fn(nextX)
          if (y1 * y2 <= 0) loop(nextX, (x, nextX) :: acc)
          else loop(nextX, acc)
        }
      }
    }

    Try {
      loop(interval._1, Nil)
    }.toEither.left.map(_ => EvaluationError("Function evaluation failed"))
  }

  // Уточнение корней методом деления пополам
  private def refineRoots(
    fn: Function,
    intervals: List[(Double, Double)],
    tolerance: Double,
    maxIterations: Int
  ): Either[ExtremaError, List[Double]] = {
    def solveInterval(a: Double, b: Double): Either[ExtremaError, Double] = {
      @tailrec
      def iterate(a: Double, b: Double, iterations: Int): Either[ExtremaError, Double] = {
        if (iterations >= maxIterations) Left(NoConvergence(maxIterations))
        else {
          val mid = (a + b) / 2
          val fa = fn(a)
          val fmid = fn(mid)
          
          if (abs(fmid) < tolerance) Right(mid)
          else if (fa * fmid < 0) iterate(a, mid, iterations + 1)
          else iterate(mid, b, iterations + 1)
        }
      }
      
      iterate(a, b, 0)
    }

    intervals.foldLeft(Right(Nil): Either[ExtremaError, List[Double]]) {
      case (acc, (a, b)) =>
        for {
          roots <- acc
          root <- solveInterval(a, b)
        } yield root :: roots
    }.map(_.reverse)
  }

  // Дополнительный метод для классификации экстремумов
  def classifyExtremum(
    expr: Expr,
    variable: String,
    point: Double,
    epsilon: Double = 1e-4
  ): Either[ExtremaError, String] = {
    val secondDerivative = Differentiator.differentiate(
      Differentiator.differentiate(expr, variable),
      variable
    )
    val secondDerivFn = buildFunction(secondDerivative, variable)
    
    Try {
      val derivValue = secondDerivFn(point)
      if (derivValue > 0) "local minimum"
      else if (derivValue < 0) "local maximum"
      else "possible inflection point"
    }.toEither.left.map(_ => EvaluationError("Failed to classify extremum"))
  }
}
```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.