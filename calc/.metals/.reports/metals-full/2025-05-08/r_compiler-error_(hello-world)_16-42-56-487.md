file:///C:/Users/artam/Desktop/Scala%202.0/hello-world/src/main/scala/Main2.scala
### java.nio.file.InvalidPathException: Illegal char <:> at index 3: jar:file:///C:/Users/artam/AppData/Local/Coursier/cache/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.15/scala-library-2.13.15-sources.jar!/scala/collection/immutable/List.scala

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 925
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

def parseNumber(tokens: List[String]): Double = {
    tokens m@@

}
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

java.nio.file.InvalidPathException: Illegal char <:> at index 3: jar:file:///C:/Users/artam/AppData/Local/Coursier/cache/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.15/scala-library-2.13.15-sources.jar!/scala/collection/immutable/List.scala