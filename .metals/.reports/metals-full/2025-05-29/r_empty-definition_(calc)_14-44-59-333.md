error id: file:///C:/Users/artam/Desktop/Scala%202.0/Scala/calc/src/main/scala/Main.scala:`<none>`.
file:///C:/Users/artam/Desktop/Scala%202.0/Scala/calc/src/main/scala/Main.scala
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:

offset: 1984
uri: file:///C:/Users/artam/Desktop/Scala%202.0/Scala/calc/src/main/scala/Main.scala
text:
```scala
object MainProject {

import scala.math._

case class SyntaxError(message: String) extends Exception(message)


sealed trait Expr
case class Number(value: Double) extends Expr
case class Variable(name: String) extends Expr
case class BinOp(op: String, left: Expr, right: Expr) extends Expr
case class UnaryOp(func: String, arg: Expr) extends Expr

val operators = Set("+", "-", "*", "/", "^")
val functions = Set("sin", "cos", "log")


def tokenize(expr: String): List[String] = {
  val pattern = """(\d+(\.\d+)?|[a-zA-Z]+|[+\-*/^()]|\s+)""".r
  pattern.findAllIn(expr).map(_.trim).filter(_.nonEmpty).toList
}





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

def treeToString(expr: Expr): String = expr match {
  case Number(value) => value.toString
  case Variable(name) => name
  case BinOp(op, left, right) =>
    val leftStr = left match {
      case BinOp(innerOp, _, _) if precedence(innerOp) < precedence(op) => s"(${treeToString(left)})"
      case _ => treeToString(left)
    }
    val rightStr = right match {
      case BinOp(innerOp, _, _) if precedence(innerOp) <= precedence(op) && op != "^" => s"(${treeToString(right)})"
      case _ => treeToString(right)
    }
    s"$leftStr$op$rightStr"
  case UnaryOp(func, arg) =>
    val argStr = arg match {
      case BinOp(_, _, _) => s"(${treeToString(arg)})"
      case _ => treeToString(arg)
    }
    s"$func($argStr)"
}

def precedence(op: String): Int = op match {
  case "+" | "-" => 1
  case "*" | "/" => 2
  case "^" => 3
  case _ => 0
}


@@


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


def integrate(expr: Expr, variable: String): Expr = expr match {
  case Number(c) =>
    BinOp("*", Number(c), Variable(variable))
  case Variable(name) =>
    if (name == variable)
      BinOp("/", BinOp("^", Variable(variable), Number(2.0)), Number(2.0))
    else
      BinOp("*", Variable(name), Variable(variable))
  case BinOp("+", u, v) =>
    BinOp("+", integrate(u, variable), integrate(v, variable))
  case BinOp("-", u, v) =>
    BinOp("-", integrate(u, variable), integrate(v, variable))
  case BinOp("^", Variable(v), Number(exp)) if v == variable && exp != -1 =>
    BinOp("/", BinOp("^", Variable(variable), Number(exp + 1)), Number(exp + 1))
  case UnaryOp("sin", u) if u == Variable(variable) =>
    BinOp("*", Number(-1.0), UnaryOp("cos", u))
  case UnaryOp("cos", u) if u == Variable(variable) =>
    UnaryOp("sin", u)
  case _ =>
    throw new UnsupportedOperationException(s"Cannot integrate: $expr")
}


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
  println(treeToString(derivative))
  

  val integral = integrate(tree, "x")
  println("\nIntegral:")
  printTree(integral)
  println(treeToString(integral))
  
}

}

```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.