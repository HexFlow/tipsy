package gala.compiler

sealed trait WorkflowCompilationError

case class WorkflowLexerError(location: Location, msg: String) extends WorkflowCompilationError
case class WorkflowParserError(location: Location, msg: String) extends WorkflowCompilationError

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}

sealed trait CCompilationError

case class CLexerError(location: Location, msg: String) extends CCompilationError
case class CParserError(location: Location, msg: String) extends CCompilationError
