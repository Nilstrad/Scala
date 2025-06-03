import scala.math._
import Algebra._

object Evaluator {
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
    case Number(_) => Number(0)
    case Variable(name) if name == variable => Number(1)
    case Variable(_) => Number(0)
    
    case UnaryOp("-", e) => UnaryOp("-", differentiate(e, variable))
    
    case BinOp("+", l, r) => BinOp("+", differentiate(l, variable), differentiate(r, variable))
    case BinOp("-", l, r) => BinOp("-", differentiate(l, variable), differentiate(r, variable))
    
    case BinOp("*", l, r) =>
      BinOp("+",
        BinOp("*", differentiate(l, variable), r),
        BinOp("*", l, differentiate(r, variable))
      )
    
    case BinOp("/", l, r) =>
      BinOp("/",
        BinOp("-",
          BinOp("*", differentiate(l, variable), r),
          BinOp("*", l, differentiate(r, variable))
        ),
        BinOp("^", r, Number(2))
      )
    
    case BinOp("^", base, Number(exp)) =>
      BinOp("*",
        BinOp("*", Number(exp), BinOp("^", base, Number(exp - 1))),
        differentiate(base, variable)
      )
    
    case UnaryOp("sin", arg) => BinOp("*", UnaryOp("cos", arg), differentiate(arg, variable))
    case UnaryOp("cos", arg) => UnaryOp("-", BinOp("*", UnaryOp("sin", arg), differentiate(arg, variable)))
    
    case _ => throw new IllegalArgumentException("Unsupported expression for differentiation")
  }

  def integrate(expr: Expr, variable: String): Expr = expr match {
    case Number(c) => BinOp("*", Number(c), Variable(variable))
    case Variable(name) if name == variable => 
      BinOp("/", BinOp("^", Variable(variable), Number(2)), Number(2))
    case Variable(name) => BinOp("*", Variable(name), Variable(variable))
    
    case BinOp("+", u, v) => BinOp("+", integrate(u, variable), integrate(v, variable))
    case BinOp("-", u, v) => BinOp("-", integrate(u, variable), integrate(v, variable))
    
    case BinOp("*", Number(c), Variable(name)) if name == variable =>
      BinOp("*", Number(c), BinOp("/", BinOp("^", Variable(variable), Number(2)), Number(2)))
    
    case BinOp("^", Variable(name), Number(n)) if name == variable && n != -1 =>
      BinOp("/", BinOp("^", Variable(variable), Number(n + 1)), Number(n + 1))
    
    case UnaryOp("sin", Variable(name)) if name == variable =>
      BinOp("*", Number(-1), UnaryOp("cos", Variable(variable)))
    
    case UnaryOp("cos", Variable(name)) if name == variable =>
      UnaryOp("sin", Variable(variable))
    
    case BinOp("*", Number(a), UnaryOp("sin", BinOp("*", Number(b), Variable(name)))) 
         if name == variable =>
      BinOp("*", Number(-a/b), UnaryOp("cos", BinOp("*", Number(b), Variable(variable))))
    
    case BinOp("*", Number(a), UnaryOp("cos", BinOp("*", Number(b), Variable(name)))) 
         if name == variable =>
      BinOp("*", Number(a/b), UnaryOp("sin", BinOp("*", Number(b), Variable(variable))))
    
    case _ => throw new UnsupportedOperationException(s"Cannot integrate: $expr")
  }
}
