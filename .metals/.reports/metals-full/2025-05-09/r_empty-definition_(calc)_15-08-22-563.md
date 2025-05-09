error id: 
file:///C:/Users/artam/Desktop/Scala%202.0/calc/src/main/scala/Main2.scala
empty definition using pc, found symbol in pc: 
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -scala/math/BinOp.
	 -scala/math/BinOp#
	 -scala/math/BinOp().
	 -BinOp.
	 -BinOp#
	 -BinOp().
	 -scala/Predef.BinOp.
	 -scala/Predef.BinOp#
	 -scala/Predef.BinOp().
offset: 5696
uri: file:///C:/Users/artam/Desktop/Scala%202.0/calc/src/main/scala/Main2.scala
text:
```scala
object MainProject{

import scala.math._

case class SyntaxError(message: String) extends Exception(message)

// === AST ===

sealed trait Expr
case class Number(value: Double) extends Expr
case class Variable(name: String) extends Expr
case class BinOp(op: String, left: Expr, right: Expr) extends Expr
case class UnaryOp(func: String, arg: Expr) extends Expr

// === Операторы и функции ===

val operators = Set("+", "-", "*", "/", "^")
val functions = Set("sin", "cos", "log")

// === Токенизация ===

def tokenize(expr: String): List[String] = {
  val pattern = """(\d+(\.\d+)?|[a-zA-Z]+|[+\-*/^()]|\s+)""".r
  pattern.findAllIn(expr).map(_.trim).filter(_.nonEmpty).toList
}

// === Парсинг ===

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

// === Вывод дерева ===

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

// === Вычисление ===

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
  case Number(_)       => Number(0)
  case Variable(name)  => if (name == variable) Number(1) else Number(0)
  case BinOp("+", u, v) => BinOp("+", differentiate(u, variable), differentiate(v, variable))
  case BinOp("-", u, v) => BinOp("-", differentiate(u, variable), differentiate(v, variable))
  case BinOp("*", u, v) =>
    BinOp("+",
      BinOp("*", differentiate(u, variable), v),
      BinOp("*", u, differentiate(v, variable))
    )
  case BinOp("/", u, v) =>
    BinOp("/",
      BinOp("-", 
        BinOp("*", differentiate(u, variable), v),
        BinOp("*", u, differentiate(v, variable))
      ),
      BinOp("^", v, Number(2))
    )
  case BinOp("^", base, Number(expr)) =>
    BinOp("*", BinOp("*", Number(expr), BinOp("^", base, Number(expr - 1))), differentiate(base, variable))
  case UnaryOp("sin", u) =>
    BinOp("*", UnaryOp("cos", u), differentiate(u, variable))
  case UnaryOp("cos", u) =>
    BinOp("*", Number(-1), BinOp("*", UnaryOp("sin", u), differentiate(u, variable)))
  case UnaryOp("log", u) =>
    BinOp("/", differentiate(u, variable), u)
  case _ => throw new UnsupportedOperationException(s"Cannot differentiate: $expr")
}

// Интегрирование выражений по переменной
def integrate(expr: Expr, variable: String): Expr = expr match {
  // Интеграл от константы — это константа * x
  case Number(c) =>
    BinOp("*", Number(c), Variable(variable))

  // Интеграл от переменной x — это x^2 / 2
  case Variable(name) =>
    if (name == variable)
      BinOp("/", BinOp("^", Variable(variable), Number(2.0)), Number(2.0))
    else
      BinOp("*", Variable(name), Variable(variable))

  // Интеграл суммы — это сумма интегралов
  case BinOp("+", u, v) =>
    BinOp("+", integrate(u, variable), integrate(v, variable))

  // Интеграл разности — это разность интегралов
  case BinOp("-", u, v) =>
    BinOp("-", integrate(u, variable), integrate(v, variable))

  // Интеграл от x^n = x^(n+1)/(n+1), если n ≠ -1
  case @@BinOp("^", Variable(v), Number(exp)) if v == variable && exp.value != -1 =>
    BinOp("/", BinOp("^", Variable(variable), Number(exp.value + 1)), Number(exp.value + 1))

  // Интеграл sin(x) = -cos(x)
  case UnaryOp("sin", u) if u == Variable(variable) =>
    BinOp("*", Number(-1.0), UnaryOp("cos", u))

  // Интеграл cos(x) = sin(x)
  case UnaryOp("cos", u) if u == Variable(variable) =>
    UnaryOp("sin", u)

  // Всё остальное пока не поддерживается
  case _ =>
    throw new UnsupportedOperationException(s"Cannot integrate: $expr")
}


// === Пример работы ===

@main def main(): Unit = {
  val expr = "x^2 + sin(x)"
  val tokens = tokenize(expr)
  println("Tokens: " + tokens.mkString(", "))

  val (tree, rest) = parseExpression(tokens)
  if (rest.nonEmpty) println(s"Unexpected tokens: $rest")

  println("\nExpression Tree:")
  printTree(tree)

  val result = evaluate(tree, Map("x" -> 3))
  println(s"\nResult at x=3: $result")

  val derivative = differentiate(tree, "x")
  println("\nDerivative:")
  printTree(derivative)

  val integral = integrate(tree, "x")
  println("\nIntegral:")
  printTree(integral)
}
}
```


#### Short summary: 

empty definition using pc, found symbol in pc: 