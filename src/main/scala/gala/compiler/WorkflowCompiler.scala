package gala.compiler

import gala.lexer._
import gala.parser._

object WorkflowCompiler {
  def apply(code: String): Either[CCompilationError, ParseTree] = {
    for {
      tokens <- CLexer(code).right
      parseTree <- CParser(tokens).right
    } yield parseTree
  }
}
