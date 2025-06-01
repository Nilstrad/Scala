import scala.math._

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
}
