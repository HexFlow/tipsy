package gala.compiler

import gala.lexer._
import gala.parser._

object WorkflowCompiler {
  def apply(code: String): Either[CCompilationError, List[CToken]] = {
    for {
      tokens <- CLexer(code).right
      parseTree <- CParser(tokens).right
    } yield tokens
  }
}
