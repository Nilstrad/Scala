error id: file:///C:/Users/artam/Desktop/Scala%202.0/Scala/calc/src/main/scala/Differentiator.scala:`<none>`.
file:///C:/Users/artam/Desktop/Scala%202.0/Scala/calc/src/main/scala/Differentiator.scala
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:

offset: 95
uri: file:///C:/Users/artam/Desktop/Scala%202.0/Scala/calc/src/main/scala/Differentiator.scala
text:
```scala
object Differentiator {
def differentiate(expr: Expr, variable: String): Expr = expr match {
  @@case Number(_)       => Number(0)
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
  case BinOp("^", base, Number(exp)) =>
    BinOp("*", BinOp("*", Number(exp), BinOp("^", base, Number(exp - 1))), differentiate(base, variable))
  case UnaryOp("sin", u) =>
    BinOp("*", UnaryOp("cos", u), differentiate(u, variable))
  case UnaryOp("cos", u) =>
    BinOp("*", Number(-1), BinOp("*", UnaryOp("sin", u), differentiate(u, variable)))
  case UnaryOp("log", u) =>
    BinOp("/", differentiate(u, variable), u)
  case _ => throw new UnsupportedOperationException(s"Cannot differentiate: $expr")
}
}

```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.