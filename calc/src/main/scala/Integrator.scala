object Integrator {
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
}
