import scala.io.Source
import scala.util.{Try, Using}

object ConfigReader {
  case class Config(expression: String, variable: String, values: List[Double])
  
  sealed trait ConfigError
  case class FileNotFound(path: String) extends ConfigError
  case class InvalidConfig(reason: String) extends ConfigError
  case class ParseError(details: String) extends ConfigError

  def readConfig(filename: String): Either[ConfigError, Config] = {
    readFile(filename)
      .flatMap(parseConfig)
  }

  private def readFile(filename: String): Either[ConfigError, List[String]] = {
    Using(Source.fromFile(filename)) { source =>
      source.getLines().toList
        .map(_.trim)
        .filter(_.nonEmpty)
    }.toEither
      .left.map(_ => FileNotFound(filename))
  }

  private def parseConfig(lines: List[String]): Either[ConfigError, Config] = {
    lines match {
      case Nil => Left(InvalidConfig("Empty config file"))
      case expr :: rest =>
        for {
          varValues <- parseVariableValues(rest)
          (variable, values) = varValues
        } yield Config(expr, variable, values)
    }
  }

  private def parseVariableValues(lines: List[String]): Either[ConfigError, (String, List[Double])] = {
    lines.flatMap(parseVariableLine).headOption
      .toRight(InvalidConfig("No valid variable definitions found"))
  }

  private def parseVariableLine(line: String): Option[(String, List[Double])] = {
    val parts = line.split("=").map(_.trim)
    if (parts.length != 2) None
    else {
      val variable = parts(0)
      val values = parts(1).split(",")
        .map(_.trim)
        .flatMap(s => Try(s.toDouble).toOption)
        .toList
      
      if (values.nonEmpty) Some((variable, values))
      else None
    }
  }
}