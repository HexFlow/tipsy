package tipsy.compiler

import tipsy.lexer._
import tipsy.parser._

object WorkflowCompiler {
  def apply(code: String): Either[CCompilationError, ParseTree] = {
    for {
      tokens <- CLexer(code).right
      parseTree <- CPackParser(tokens).right
    } yield parseTree
  }

  def getTokens(code: String): Either[CCompilationError, List[CToken]] = {
    for {
      tokens <- CLexer(code).right
    } yield tokens
  }
}
