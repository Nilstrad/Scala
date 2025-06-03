object Algebra {
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
}
