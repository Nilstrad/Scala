object Parser {
  import Algebra._
  import Algebra.Expr

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

  private def parseBinary(tokens: List[String], nextLevel: List[String] => (Expr, List[String]), ops: Set[String]): (Expr, List[String]) = {
    var (left, rest) = nextLevel(tokens)
    while (rest.nonEmpty && ops.contains(rest.head)) {
      val op = rest.head
      val (right, restNext) = nextLevel(rest.tail)
      left = BinOp(op, left, right)
      rest = restNext
    }
    (left, rest)
  }

  private def parseFactor(tokens: List[String]): (Expr, List[String]) = {
    var (left, rest) = parseUnary(tokens)
    while (rest.nonEmpty && rest.head == "^") {
      val (right, restNext) = parseUnary(rest.tail)
      left = BinOp("^", left, right)
      rest = restNext
    }
    (left, rest)
  }

  private def parseUnary(tokens: List[String]): (Expr, List[String]) = tokens match {
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
