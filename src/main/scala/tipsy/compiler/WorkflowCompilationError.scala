package tipsy.compiler

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}

sealed trait CCompilationError

case class CPreError(msg: String) extends CCompilationError
case class CLexerError(location: Location, msg: String) extends CCompilationError
case class CParserError(location: Location, msg: String) extends CCompilationError
