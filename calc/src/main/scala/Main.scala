object MainProject {

import scala.math._
import scala.io.Source
import scala.util.{Try, Success, Failure}

// Tagless Final подход - определяем алгебру для наших операций
trait ExpressionAlgebra[F[_]] {
  def number(value: Double): F[Double]
  def variable(name: String): F[Double]
  def add(left: F[Double], right: F[Double]): F[Double]
  def subtract(left: F[Double], right: F[Double]): F[Double]
  def multiply(left: F[Double], right: F[Double]): F[Double]
  def divide(left: F[Double], right: F[Double]): F[Double]
  def power(base: F[Double], exponent: F[Double]): F[Double]
  def sin(arg: F[Double]): F[Double]
  def cos(arg: F[Double]): F[Double]
  def log(arg: F[Double]): F[Double]
}

type ExprF[A] = Expr

// Реализация интерпретатора для Tagless Final
object ExpressionInterpreter extends ExpressionAlgebra[ExprF] {
  def number(value: Double): Expr = Number(value)
  def variable(name: String): Expr = Variable(name)
  def add(left: Expr, right: Expr): Expr = BinOp("+", left, right)
  def subtract(left: Expr, right: Expr): Expr = BinOp("-", left, right)
  def multiply(left: Expr, right: Expr): Expr = BinOp("*", left, right)
  def divide(left: Expr, right: Expr): Expr = BinOp("/", left, right)
  def power(base: Expr, exponent: Expr): Expr = BinOp("^", base, exponent)
  def sin(arg: Expr): Expr = UnaryOp("sin", arg)
  def cos(arg: Expr): Expr = UnaryOp("cos", arg)
  def log(arg: Expr): Expr = UnaryOp("log", arg)
}

case class SyntaxError(message: String) extends Exception(message)
case class ConfigError(message: String) extends Exception(message)

sealed trait Expr
case class Number(value: Double) extends Expr
case class Variable(name: String) extends Expr
case class BinOp(op: String, left: Expr, right: Expr) extends Expr
case class UnaryOp(func: String, arg: Expr) extends Expr

val operators = Set("+", "-", "*", "/", "^")
val functions = Set("sin", "cos", "log")


def tokenize(expr: String): List[String] = {
  val pattern = """(\d+(\.\d+)?|[a-zA-Z]+|[+\-*/^()]|\s+)""".r
  pattern.findAllIn(expr).map(_.trim).filter(_.nonEmpty).toList
}


def parseExpression(tokens: List[String]): (Expr, List[String]) = {
  parseBinary(tokens, parseTerm, Set("+", "-"))
}

def parseTerm(tokens: List[String]): (Expr, List[String]) = {
  parseBinary(tokens, parseFactor, Set("*", "/"))
}

def parseBinary(tokens: List[String], nextLevel: List[String] => (Expr, List[String]), ops: Set[String]): (Expr, List[String]) = {
  var (left, rest) = nextLevel(tokens)
  while (rest.nonEmpty && ops.contains(rest.head)) {
    val op = rest.head
    val (right, restNext) = nextLevel(rest.tail)
    left = BinOp(op, left, right)
    rest = restNext
  }
  (left, rest)
}

def parseFactor(tokens: List[String]): (Expr, List[String]) = {
  var (left, rest) = parseUnary(tokens)
  while (rest.nonEmpty && rest.head == "^") {
    val (right, restNext) = parseUnary(rest.tail)
    left = BinOp("^", left, right)
    rest = restNext
  }
  (left, rest)
}

def parseUnary(tokens: List[String]): (Expr, List[String]) = tokens match {
  case head :: tail if functions.contains(head) =>
    tail match {
      case "(" :: rest =>
        val (arg, rest2) = parseExpression(rest)
        rest2 match {
          case ")" :: rest3 => (UnaryOp(head, arg), rest3)
          case _            => throw SyntaxError("Expected ')' after function argument")
        }
      case _ => throw SyntaxError(s"Expected '(' after $head")
    }
  case "(" :: rest =>
    val (expr, rest2) = parseExpression(rest)
    rest2 match {
      case ")" :: rest3 => (expr, rest3)
      case _            => throw SyntaxError("Expected ')'")
    }
  case number :: rest if number.matches("""\d+(\.\d+)?""") =>
    (Number(number.toDouble), rest)
  case variable :: rest if variable.matches("""[a-zA-Z]+""") =>
    (Variable(variable), rest)
  case _ => throw SyntaxError(s"Unexpected token: $tokens")
}


def printTree(node: Expr, indent: Int = 0): Unit = {
  val pad = " " * indent
  node match {
    case Number(v)          => println(s"$pad Number($v)")
    case Variable(name)     => println(s"$pad Variable($name)")
    case BinOp(op, l, r)    =>
      println(s"$pad BinOp($op)")
      printTree(l, indent + 2)
      printTree(r, indent + 2)
    case UnaryOp(func, arg) =>
      println(s"$pad Unary($func)")
      printTree(arg, indent + 2)
  }
}

def treeToString(expr: Expr): String = expr match {
  case Number(value) => value.toString
  case Variable(name) => name
  case BinOp(op, left, right) =>
    val leftStr = left match {
      case BinOp(innerOp, _, _) if precedence(innerOp) < precedence(op) => s"(${treeToString(left)})"
      case _ => treeToString(left)
    }
    val rightStr = right match {
      case BinOp(innerOp, _, _) if precedence(innerOp) <= precedence(op) && op != "^" => s"(${treeToString(right)})"
      case _ => treeToString(right)
    }
    s"$leftStr$op$rightStr"
  case UnaryOp(func, arg) =>
    val argStr = arg match {
      case BinOp(_, _, _) => s"(${treeToString(arg)})"
      case _ => treeToString(arg)
    }
    s"$func($argStr)"
}

def precedence(op: String): Int = op match {
  case "+" | "-" => 1
  case "*" | "/" => 2
  case "^" => 3
  case _ => 0
}


def evaluate(node: Expr, variables: Map[String, Double] = Map()): Double = node match {
  case Number(v) => v
  case Variable(name) =>
    variables.getOrElse(name, throw new IllegalArgumentException(s"Variable '$name' not provided"))
  case BinOp(op, l, r) =>
    val a = evaluate(l, variables)
    val b = evaluate(r, variables)
    op match {
      case "+" => a + b
      case "-" => a - b
      case "*" => a * b
      case "/" => a / b
      case "^" => pow(a, b)
    }
  case UnaryOp(func, arg) =>
    func match {
      case "sin" => sin(evaluate(arg, variables))
      case "cos" => cos(evaluate(arg, variables))
      case "log" => log(evaluate(arg, variables))
    }
}


def differentiate(expr: Expr, variable: String): Expr = expr match {
  case Number(_) => Number(0)
  case Variable(name) if name == variable => Number(1)
  case Variable(_) => Number(0)
  
  case UnaryOp("-", e) => UnaryOp("-", differentiate(e, variable))
  
  case BinOp("+", l, r) => BinOp("+", differentiate(l, variable), differentiate(r, variable))
  case BinOp("-", l, r) => BinOp("-", differentiate(l, variable), differentiate(r, variable))
  
  case BinOp("*", l, r) =>
    BinOp("+",
      BinOp("*", differentiate(l, variable), r),
      BinOp("*", l, differentiate(r, variable))
    )
  
  case BinOp("/", l, r) =>
    BinOp("/",
      BinOp("-",
        BinOp("*", differentiate(l, variable), r),
        BinOp("*", l, differentiate(r, variable))
      ),
      BinOp("^", r, Number(2))
    )
  
  case BinOp("^", base, Number(exp)) =>
    BinOp("*",
      BinOp("*", Number(exp), BinOp("^", base, Number(exp - 1))),
      differentiate(base, variable)
    )
  
  case UnaryOp("sin", arg) => BinOp("*", UnaryOp("cos", arg), differentiate(arg, variable))
  case UnaryOp("cos", arg) => UnaryOp("-", BinOp("*", UnaryOp("sin", arg), differentiate(arg, variable)))
  
  case _ => throw new IllegalArgumentException("Unsupported expression for differentiation")
}


def integrate(expr: Expr, variable: String): Expr = expr match {
  case Number(c) => BinOp("*", Number(c), Variable(variable))
  case Variable(name) if name == variable => 
    BinOp("/", BinOp("^", Variable(variable), Number(2)), Number(2))
  case Variable(name) => BinOp("*", Variable(name), Variable(variable))
  
  case BinOp("+", u, v) => BinOp("+", integrate(u, variable), integrate(v, variable))
  case BinOp("-", u, v) => BinOp("-", integrate(u, variable), integrate(v, variable))
  
  case BinOp("*", Number(c), Variable(name)) if name == variable =>
    BinOp("*", Number(c), BinOp("/", BinOp("^", Variable(variable), Number(2)), Number(2)))
  
  case BinOp("^", Variable(name), Number(n)) if name == variable && n != -1 =>
    BinOp("/", BinOp("^", Variable(variable), Number(n + 1)), Number(n + 1))
  
  case UnaryOp("sin", Variable(name)) if name == variable =>
    BinOp("*", Number(-1), UnaryOp("cos", Variable(variable)))
  
  case UnaryOp("cos", Variable(name)) if name == variable =>
    UnaryOp("sin", Variable(variable))
  
  case BinOp("*", Number(a), UnaryOp("sin", BinOp("*", Number(b), Variable(name)))) 
       if name == variable =>
    BinOp("*", Number(-a/b), UnaryOp("cos", BinOp("*", Number(b), Variable(variable))))
  
  case BinOp("*", Number(a), UnaryOp("cos", BinOp("*", Number(b), Variable(name)))) 
       if name == variable =>
    BinOp("*", Number(a/b), UnaryOp("sin", BinOp("*", Number(b), Variable(variable))))
  
  case _ => throw new UnsupportedOperationException(s"Cannot integrate: $expr")
}

def findExtremumPoints(expr: Expr, variable: String, start: Double = -100, end: Double = 100, step: Double = 0.1): List[Double] = {
  val derivative = differentiate(expr, variable)
  
  def findRoots(): List[Double] = {
    (BigDecimal(start) to BigDecimal(end) by BigDecimal(step)).map(_.toDouble).sliding(2).flatMap { case Seq(x1, x2) =>
      try {
        val y1 = evaluate(derivative, Map(variable -> x1))
        val y2 = evaluate(derivative, Map(variable -> x2))
        
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
  
  // Filter out points where derivative is actually zero (not just crossing zero)
  findRoots().filter { x =>
    try {
      val y = evaluate(derivative, Map(variable -> x))
      abs(y) < 1e-6
    } catch {
      case _: Exception => false
    }
  }
}

def readConfigFile(filename: String): Try[(String, String, List[(String, Double)])] = Try {
  val lines = Source.fromFile(filename).getLines().map(_.trim).filter(_.nonEmpty).toList
  if (lines.size < 2) throw ConfigError("Config file must contain at least 2 lines (variable and expression)")

  val variable = lines.head
  if (!variable.matches("^[a-zA-Z]+$")) throw ConfigError(s"Invalid variable name: $variable")

  val expr = lines(1)
  val variableValues = lines.drop(2).map { line =>
    if (!line.contains("=")) throw ConfigError(s"Invalid variable assignment format: $line")
    val parts = line.split("=").map(_.trim)
    if (parts.length != 2) throw ConfigError(s"Invalid variable assignment: $line")
    if (parts(0) != variable) throw ConfigError(s"Assignment to wrong variable: expected $variable, got ${parts(0)}")
    (parts(0), Try(parts(1).toDouble).getOrElse(throw ConfigError(s"Invalid number format: ${parts(1)}")))
  }

  (variable, expr, if (variableValues.nonEmpty) variableValues else List((variable, 0.0)))
}

def processExpressionFromConfig(filename: String): Unit = {
  readConfigFile(filename) match {
    case Success((variable, exprStr, variableValues)) =>
      val tokens = tokenize(exprStr)
      println(s"Variable: $variable")
      println(s"Processing expression: $exprStr")
      println("Tokens: " + tokens.mkString(", "))

      val (tree, rest) = parseExpression(tokens)
      if (rest.nonEmpty) println(s"Warning: Unexpected tokens remaining after parsing: $rest")

      println("\nExpression Tree:")
      printTree(tree)

      // Evaluation
      println("\nEvaluation Results:")
      variableValues.foreach { case (varName, value) =>
        try {
          val result = evaluate(tree, Map(varName -> value))
          println(s"f($varName)=$value: $result")
        } catch {
          case e: IllegalArgumentException =>
            println(s"Error evaluating at $varName=$value: ${e.getMessage}")
        }
      }

      // Differentiation
      val derivative = differentiate(tree, variable)
      println("\nDerivative Tree:")
      printTree(derivative)
      println("\nDerivative Expression: " + treeToString(derivative))

      println("\nDerivative Values:")
      variableValues.foreach { case (varName, value) =>
        try {
          val result = evaluate(derivative, Map(varName -> value))
          println(s"f'($varName)=$value: $result")
        } catch {
          case e: IllegalArgumentException =>
            println(s"Error evaluating derivative at $varName=$value: ${e.getMessage}")
        }
      }

      // Integration
      try {
        val integral = integrate(tree, variable)
        println("\nIntegral Tree:")
        printTree(integral)
        println("\nIntegral Expression: " + treeToString(integral))

        println("\nIntegral Values:")
        variableValues.foreach { case (varName, value) =>
          try {
            val result = evaluate(integral, Map(varName -> value))
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
        val extremumPoints = findExtremumPoints(tree, variable)
        if (extremumPoints.nonEmpty) {
          println("\nExtremum Points (approximate):")
          extremumPoints.foreach { x =>
            try {
              val y = evaluate(tree, Map(variable -> x))
              val d2 = evaluate(differentiate(derivative, variable), Map(variable -> x))
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