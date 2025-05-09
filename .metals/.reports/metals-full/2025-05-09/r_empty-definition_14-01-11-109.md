error id: `<none>`.
file:///C:/Users/artam/Desktop/Scala%202.0/calc/src/main/scala/Main2.scala
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:

offset: 4393
uri: file:///C:/Users/artam/Desktop/Scala%202.0/calc/src/main/scala/Main2.scala
text:
```scala
import scala.math._

case class SyntaxError(message: String) extends Exception(message)

def tokenize(expr: String, tokens: List[String] = Nil, current: String = ""): List[String] = {
  if (expr.isEmpty) {
    if (current.nonEmpty) tokens :+ current
    else tokens
  } else {
    val (head, tail) = expr.splitAt(1)

    head match {
      case " " =>
        tokenize(tail, if (current.nonEmpty) tokens :+ current else tokens, "")
      case "+" | "-" | "*" | "/" | "^" | "(" | ")" =>
        val newTokens = if (current.nonEmpty) tokens :+ current else tokens
        tokenize(tail, newTokens :+ head, "")
      case h if h.head.isLetter =>
        tokenize(tail, tokens, current + h)
      case h if h.head.isDigit || (h == "." && current.nonEmpty && current.last.isDigit) =>
        tokenize(tail, tokens, current + h)
      case _ =>
        throw new SyntaxError(s"Unexpected character: $head")
    }
  }
}

def parseNumber(tokens: List[String]): (Any, List[String]) = {
  tokens match {
    case Nil => throw SyntaxError("Expected a number, but found nothing.")
    case token :: rest =>
      try {
        val number = token.toDouble
        (("number", number), rest)
      } catch {
        case _: NumberFormatException => throw SyntaxError(s"Invalid number: $token")
      }
  }
}

def parseTerm(tokens: List[String]): (Any, List[String]) = {
  var (left, rest) = parsePower(tokens)

  while (rest.nonEmpty && (rest.head == "*" || rest.head == "/")) {
    val op = rest.head
    val (right, restNext) = parsePower(rest.tail)
    left = ("binop", op, left, right)
    rest = restNext
  }

  (left, rest)
}

def parsePower(tokens: List[String]): (Any, List[String]) = {
  var (left, rest) = parseFactor(tokens)

  while (rest.nonEmpty && rest.head == "^") {
    val (right, restNext) = parseFactor(rest.tail)
    left = ("binop", "^", left, right)
    rest = restNext
  }

  (left, rest)
}

def parseFactor(tokens: List[String]): (Any, List[String]) = {
  if (tokens.isEmpty) {
    throw new SyntaxError("Unexpected end of expression")
  }

  val token = tokens.head
  val rest = tokens.tail

  token match {
    case "(" =>
      val (expr, rest2) = parseExpression(rest)
      rest2 match {
        case ")" :: rest3 => (expr, rest3)
        case _            => throw new SyntaxError("Expected ')'")
      }

    case "log" | "sin" | "cos" =>
      rest match {
        case "(" :: rest2 =>
          val (arg, rest3) = parseExpression(rest2)
          rest3 match {
            case ")" :: rest4 => (("unary", token, arg), rest4)
            case _            => throw new SyntaxError("Expected ')' after function argument")
          }
        case _ => throw new SyntaxError(s"Expected '(' after $token")
      }

    case _ =>
      parseNumber(token :: rest)
  }
}

def parseExpression(tokens: List[String]): (Any, List[String]) = {
  var (left, rest) = parseTerm(tokens)

  while (rest.nonEmpty && (rest.head == "+" || rest.head == "-")) {
    val op = rest.head
    val (right, restNext) = parseTerm(rest.tail)
    left = ("binop", op, left, right)
    rest = restNext
  }

  (left, rest)
}

def printTree(node: Any, indent: Int = 0): Unit = {
  val prefix = " " * indent
  node match {
    case ("number", value) => println(s"$prefix Number($value)")
    case ("binop", op, left, right) =>
      println(s"$prefix BinOp($op)")
      printTree(left, indent + 2)
      printTree(right, indent + 2)
    case ("unary", op, arg) =>
      println(s"$prefix Unary($op)")
      printTree(arg, indent + 2)
    case _ =>
      throw new IllegalArgumentException("Unexpected node type")
  }
}

def calculate(expr: String): Double = {
  val tokens = tokenize(expr)
  val (tree, rest) = parseExpression(tokens)
  if (rest.nonEmpty) {
    throw new SyntaxError(s"Unexpected tokens: $rest")
  }
  evaluate(tree)
}

def evaluate(node: Any): Double = {
  node match {
    case ("number", value: Double) => value

    case ("binop", op: String, left, right) =>
      val a = evaluate(left)
      val b = evaluate(right)
      op match {
        case "+" => a + b
        case "-" => a - b
        case "*" => a * b
        case "/" => a / b
        case "^" => math.pow(a, b)
        case _   => throw new IllegalArgumentException(s"Unknown operator: $op")
      }

    case ("unary", func: String, arg) =>
      func match {
        case "log" => math.log(evaluate(arg))
        case "sin@@" => math.sin(evaluate(arg))
        case "cos" => math.cos(evaluate(arg))
        case _     => throw new IllegalArgumentException(s"Unknown function: $func")
      }

    case _ => throw new IllegalArgumentException(s"Unknown node type: $node")
  }
}

@main def runApp(): Unit = {
  val expr = "log(2) + sin(3) * cos(4)"
  val tokens = tokenize(expr)
  println("Tokens: " + tokens.mkString(", "))

  val (tree, rest) = parseExpression(tokens)
  if (rest.nonEmpty) {
    println(s"Unexpected tokens after parsing: $rest")
  }

  println("\nExpression Tree:")
  printTree(tree)

  val result = evaluate(tree)
  println(s"\nResult: $result")
}

```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.