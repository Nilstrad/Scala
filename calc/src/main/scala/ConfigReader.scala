import scala.io.Source
import scala.util.{Try, Success, Failure}
import Algebra._

object ConfigReader {
  def readConfigFile(filename: String): Try[(String, String, List[(String, Double)])] = Try {
    val lines = Source.fromFile(filename).getLines().map(_.trim).filter(_.nonEmpty).toList
    if (lines.size < 2) throw ConfigError("Config file must contain at least 2 lines (variable and expression)")

    val variable = lines.head
    if (!variable.matches("^[a-zA-Z]+$")) throw ConfigError(s"Invalid variable name: $variable")

    val expr = lines(1)
    val variableValues = lines.drop(2).map { line =>
      if (!line.contains("=")) throw ConfigError(s"Invalid variable assignment format: $line")
      val parts = line.split("=").map(_.trim)
      if (parts.length != 2) throw ConfigError(s"Invalid variable assignment: $line")
      if (parts(0) != variable) throw ConfigError(s"Assignment to wrong variable: expected $variable, got ${parts(0)}")
      (parts(0), Try(parts(1).toDouble).getOrElse(throw ConfigError(s"Invalid number format: ${parts(1)}")))
    }

    (variable, expr, if (variableValues.nonEmpty) variableValues else List((variable, 0.0)))
  }
}
