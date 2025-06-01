error id: file:///C:/Users/artam/Desktop/Scala%202.0/Scala/calc/src/main/scala/Parser.scala:`<none>`.
file:///C:/Users/artam/Desktop/Scala%202.0/Scala/calc/src/main/scala/Parser.scala
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:

offset: 748
uri: file:///C:/Users/artam/Desktop/Scala%202.0/Scala/calc/src/main/scala/Parser.scala
text:
```scala
import scala.math._

case class SyntaxError(message: String) extends Exception(message)

sealed trait Expr
case class Number(value: Double) extends Expr
case class Variable(name: String) extends Expr
case class BinOp(op: String, left: Expr, right: Expr) extends Expr
case class UnaryOp(func: String, arg: Expr) extends Expr

object Parser {
  val operators = Set("+", "-", "*", "/", "^")
  val functions = Set("sin", "cos", "log")

  def tokenize(expr: String): List[String] = {
    val pattern = """(\d+(\.\d+)?|[a-zA-Z]+|[+\-*/^()]|\s+)""".r
    pattern.findAllIn(expr).map(_.trim).filter(_.nonEmpty).toList
  }

  def parseExpression(tokens: List[String]): (Expr, List[String]) = {
    parseBinary(tokens, parseTerm, Set("+", "-"))
  }


    def@@ parseTerm(tokens: List[String]): (Expr, List[String]) = {
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
}
```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.