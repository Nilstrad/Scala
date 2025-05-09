file:///C:/Users/artam/Desktop/Scala%202.0/hello-world/src/main/scala/Main2.scala
### java.nio.file.InvalidPathException: Illegal char <:> at index 3: jar:file:///C:/Users/artam/AppData/Local/Coursier/cache/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.15/scala-library-2.13.15-sources.jar!/scala/Any.scala

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 4157
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
    node m@@
```



#### Error stacktrace:

```
java.base/sun.nio.fs.WindowsPathParser.normalize(WindowsPathParser.java:204)
	java.base/sun.nio.fs.WindowsPathParser.parse(WindowsPathParser.java:175)
	java.base/sun.nio.fs.WindowsPathParser.parse(WindowsPathParser.java:77)
	java.base/sun.nio.fs.WindowsPath.parse(WindowsPath.java:92)
	java.base/sun.nio.fs.WindowsFileSystem.getPath(WindowsFileSystem.java:231)
	java.base/java.nio.file.Path.of(Path.java:148)
	java.base/java.nio.file.Paths.get(Paths.java:69)
	scala.meta.io.AbsolutePath$.apply(AbsolutePath.scala:58)
	scala.meta.internal.metals.MetalsSymbolSearch.$anonfun$definitionSourceToplevels$2(MetalsSymbolSearch.scala:70)
	scala.Option.map(Option.scala:242)
	scala.meta.internal.metals.MetalsSymbolSearch.definitionSourceToplevels(MetalsSymbolSearch.scala:69)
	dotty.tools.pc.completions.CaseKeywordCompletion$.dotty$tools$pc$completions$CaseKeywordCompletion$$$sortSubclasses(MatchCaseCompletions.scala:342)
	dotty.tools.pc.completions.CaseKeywordCompletion$.matchContribute(MatchCaseCompletions.scala:292)
	dotty.tools.pc.completions.Completions.advancedCompletions(Completions.scala:351)
	dotty.tools.pc.completions.Completions.completions(Completions.scala:122)
	dotty.tools.pc.completions.CompletionProvider.completions(CompletionProvider.scala:135)
	dotty.tools.pc.ScalaPresentationCompiler.complete$$anonfun$1(ScalaPresentationCompiler.scala:150)
```
#### Short summary: 

java.nio.file.InvalidPathException: Illegal char <:> at index 3: jar:file:///C:/Users/artam/AppData/Local/Coursier/cache/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.15/scala-library-2.13.15-sources.jar!/scala/Any.scala