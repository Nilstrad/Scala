file:///C:/Users/artam/Desktop/Scala%202.0/hello-world/src/main/scala/Main2.scala
### dotty.tools.dotc.ast.Trees$UnAssignedTypeException: type of Ident(p) is not assigned

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 4238
uri: file:///C:/Users/artam/Desktop/Scala%202.0/hello-world/src/main/scala/Main2.scala
text:
```scala
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
      case h if h.head.isDigit || (h == "." && current.nonEmpty && current.last.isDigit) =>
        tokenize(tail, tokens, current + h)
      case "+" | "-" | "*" | "/" | "^" | "(" | ")" =>
        tokenize(tail, if (current.nonEmpty) tokens :+ current else tokens :+ head, "")
      case h if h.head.isLetter =>
        tokenize(tail, tokens, current + h)
      case _ =>
        throw new SyntaxError(s"Unexpected character: $head")
    }
  }
}

def parseNumber(tokens: List[String]): (Double, List[String]) = {
    tokens match {
        case Nil => throw SyntaxError("Expected a number, but found nothing.")
    case token :: rest =>
      try {
        val number = token.toDouble
        (number, rest)
      } catch {
        case _: NumberFormatException => throw SyntaxError(s"Invalid number: $token")
      }
    }
}

def parseTerm(tokens: List[String]): (Any, List[String]) = {
    val (left, rest1) = parsePower(tokens)
    
    val (finalLeft, rest2) = rest1.foldLeft((left, rest1)) { case ((leftAcc, restAcc), token) =>
        token match {
            case "*" | "/" =>
                val (right, restNext) = parsePower(restAcc)
                val updateLeft = ("binop", token, leftAcc, right)
                (updateLeft, restNext)
            case _ => (leftAcc, restAcc)
        }
    }

    (finalLeft, rest2)
}

def parsePower(tokens: List[String]): (Any, List[String]) = {
  val (left, rest1) = parseFactor(tokens)

  rest1 match {
    case "^" :: rest2 =>
      val (right, rest3) = parsePower(rest2)
      (("binop", "^", left, right), rest3)
    case _ => (left, rest1)
  }
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
        case _ => throw new SyntaxError("Expected ')'")
      }

    case "log" | "sin" | "cos" =>
      rest match {
        case "(" :: rest2 =>
          val (arg, rest3) = parseExpression(rest2)
          rest3 match {
            case ")" :: rest4 => (("unary", token, arg), rest4)
            case _ => throw new SyntaxError("Expected ')' after function argument")
          }
        case _ => throw new SyntaxError(s"Expected '(' after $token")
      }

    case _ =>
      parseNumber(token :: rest)
  }
}

def parseExpression(tokens: List[String]): (Any, List[String]) = {
    val (left, rest) = parseTerm(tokens)
    rest.foldLeft((left, rest)) { case ((leftAcc, restAcc), token) =>
        token match {
            case "+" | "-" =>
                val (right, restNext) = parseTerm(restAcc)
                val updateLeft = ("binop", token, leftAcc, right)
                (updateLeft, restNext)
            case _ => (leftAcc, restAcc)
        }
    }
}

def printTree(node: Any, indent:Int = 0): Unit = {
    val prefix = " " * indent
    node match {
        case ("number", value) => println(s"$prefix Number($value)")
        case ("binop", op, left, right) => 
            println(s"$prefix BinOp($op)")
            printTree(left, indent + 1)
            printTree(right, indent + 1)
        case ("unary", op, arg) =>
             println(s"$prefix Unary($op)")
             printTree(arg, indent + 1)
        case _ => 
            throw new IllegalArgumentException("Unextpected node type")

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

        case ("binop", p[@@])

    }
```



#### Error stacktrace:

```
dotty.tools.dotc.ast.Trees$Tree.tpe(Trees.scala:74)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:208)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:104)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:88)
	dotty.tools.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:46)
	dotty.tools.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:435)
```
#### Short summary: 

dotty.tools.dotc.ast.Trees$UnAssignedTypeException: type of Ident(p) is not assigned