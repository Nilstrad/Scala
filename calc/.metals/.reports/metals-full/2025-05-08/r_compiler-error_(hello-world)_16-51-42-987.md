file:///C:/Users/artam/Desktop/Scala%202.0/hello-world/src/main/scala/Main2.scala
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 1522
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
                val (@@)
        }
}
```



#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:129)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:128)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:244)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:101)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:88)
	dotty.tools.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:46)
	dotty.tools.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:435)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: -1