import scala.util.{Success, Failure, Try}


object Main {
  def processExpressionFromConfig(filename: String): Unit = {
    ConfigReader.readConfigFile(filename) match {
      case Success((variable, exprStr, variableValues)) =>
        val tokens = Parser.tokenize(exprStr)
        println(s"Variable: $variable")
        println(s"Processing expression: $exprStr")
        println("Tokens: " + tokens.mkString(", "))

        val (tree, rest) = Parser.parseExpression(tokens)
        if (rest.nonEmpty) println(s"Warning: Unexpected tokens remaining after parsing: $rest")

        println("\nExpression Tree:")
        Printer.printTree(tree)

        // Evaluation
        println("\nEvaluation Results:")
        variableValues.foreach { case (varName, value) =>
          try {
            val result = Evaluator.evaluate(tree, Map(varName -> value))
            println(s"f($varName)=$value: $result")
          } catch {
            case e: IllegalArgumentException =>
              println(s"Error evaluating at $varName=$value: ${e.getMessage}")
          }
        }

        // Differentiation
        val derivative = Evaluator.differentiate(tree, variable)
        println("\nDerivative Tree:")
        Printer.printTree(derivative)
        println("\nDerivative Expression: " + Printer.treeToString(derivative))

        println("\nDerivative Values:")
        variableValues.foreach { case (varName, value) =>
          try {
            val result = Evaluator.evaluate(derivative, Map(varName -> value))
            println(s"f'($varName)=$value: $result")
          } catch {
            case e: IllegalArgumentException =>
              println(s"Error evaluating derivative at $varName=$value: ${e.getMessage}")
          }
        }

        // Integration
        try {
          val integral = Evaluator.integrate(tree, variable)
          println("\nIntegral Tree:")
          Printer.printTree(integral)
          println("\nIntegral Expression: " + Printer.treeToString(integral))

          println("\nIntegral Values:")
          variableValues.foreach { case (varName, value) =>
            try {
              val result = Evaluator.evaluate(integral, Map(varName -> value))
              println(s"F($varName)=$value: $result")
            } catch {
              case e: IllegalArgumentException =>
                println(s"Error evaluating integral at $varName=$value: ${e.getMessage}")
            }
          }
        } catch {
          case e: UnsupportedOperationException =>
            println(s"\nCannot calculate integral for this expression: ${e.getMessage}")
        }

        // Extremum points
        try {
          val extremumPoints = ExtremumFinder.findExtremumPoints(tree, variable)
          if (extremumPoints.nonEmpty) {
            println("\nExtremum Points (approximate):")
            extremumPoints.foreach { x =>
              try {
                val y = Evaluator.evaluate(tree, Map(variable -> x))
                val d2 = Evaluator.evaluate(Evaluator.differentiate(derivative, variable), Map(variable -> x))
                val extremumType = if (d2 > 0) "minimum" else if (d2 < 0) "maximum" else "possible inflection"
                println(f"$variable=$x%.4f, f($variable)=$y%.4f ($extremumType)")
              } catch {
                case _: Exception => println(f"Found extremum at $x%.4f but couldn't evaluate function")
              }
            }
          } else {
            println("\nNo extremum points found in the specified range")
          }
        } catch {
          case e: Exception =>
            println(s"\nError finding extremum points: ${e.getMessage}")
        }

      case Failure(e) =>
        println(s"Error processing config file: ${e.getMessage}")
    }
  }

  @main def main(): Unit = {
    processExpressionFromConfig("config123.txt")
  }
}