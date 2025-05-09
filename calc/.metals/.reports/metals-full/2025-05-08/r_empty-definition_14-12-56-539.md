error id: `<none>`.
file:///C:/Users/artam/Desktop/Scala%202.0/hello-world/src/main/scala/Main.scala
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:

offset: 3252
uri: file:///C:/Users/artam/Desktop/Scala%202.0/hello-world/src/main/scala/Main.scala
text:
```scala
sealed trait Expr

case class Number(value: Double) extends Expr
case class Variable(name: String) extends Expr
case class Sum(left: Expr, right: Expr) extends Expr
case class Product(left: Expr, right: Expr) extends Expr
case class Division(left: Expr, right: Expr) extends Expr
case class Minus(left: Expr, right: Expr) extends Expr
case class Power(left: Expr, right: Expr) extends Expr

sealed trait ParseResult[+A]
case class Success[+A](result: A, remaining: String) extends ParseResult[A]
case class Failure(error: String) extends ParseResult[Nothing]

object MathParser {

  def number(input: String): ParseResult[Double] = {
    val regex = "^[0-9]+(\\.[0-9]+)?".r
    regex.findPrefixOf(input) match {
      case Some(numStr) => Success(numStr.toDouble, input.drop(numStr.length))
      case None => Failure("Expected a number")
    }
  }

  def operator(input: Char): ParseResult[(Double, Double) => Double] = input match {
    case '+' => Success((a, b) => a + b, "")
    case '-' => Success((a, b) => a - b, "")
    case '*' => Success((a, b) => a * b, "")
    case '/' => Success((a, b) => a / b, "")
    case _ => Failure("Expected an operator (+, -, *, /)")
  }

  def term(input: String): ParseResult[Double] = {
    val factor = number(input)
    factor match {
      case Success(left, remaining) =>
        remaining.headOption match {
          case Some('*') | Some('/') =>
            val op = operator(remaining.head)
            op match {
              case Success(opFunc, rest) =>
                val right = number(rest)
                right match {
                  case Success(rightValue, restAfterOp) =>
                    val result = opFunc(left, rightValue)
                    Success(result, restAfterOp)
                  case Failure(msg) => Failure(msg)
                }
              case Failure(msg) => Failure(msg)
            }
          case _ => Success(left, remaining) 
        }
      case Failure(msg) => Failure(msg)
    }
  }

  def expr(input: String): ParseResult[Double] = {
    val firstTerm = term(input)
    firstTerm match {
      case Success(left, remaining) =>
        remaining.headOption match {
          case Some('+') | Some('-') =>
            val op = operator(remaining.head)
            op match {
              case Success(opFunc, rest) =>
                val nextTerm = term(rest)
                nextTerm match {
                  case Success(rightValue, restAfterOp) =>
                    val result = opFunc(left, rightValue)
                    expr(restAfterOp) match {
                      case Success(finalResult, _) => Success(finalResult, "")
                      case Failure(_) => Success(result, restAfterOp)
                    }
                  case Failure(msg) => Failure(msg)
                }
              case Failure(msg) => Failure(msg)
            }
          case _ => Success(left, remaining)
        }
      case Failure(msg) => Failure(msg)
    }
  }

  def parse(input: String): ParseResult[Double] = {
    expr(input)
  }
}

@main def main(): Unit = {
  val input = "2 + 3 * 4 - 5 / 2"
  val result = MathParser.parse(input)
result match {
  ca@@se Success(value, _) => println(s"Результат: $value")
  case Failure(error) => println(s"Ошибка: $error")
}
}
```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.