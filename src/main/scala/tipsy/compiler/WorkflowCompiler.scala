package tipsy.compiler

import tipsy.lexer._
import tipsy.parser._

object WorkflowCompiler {
  def apply(filename: String): Either[CCompilationError, ParseTree] = {
    for {
      code <- Preprocessor(filename).right
      tokens <- CLexer(code).right
      parseTree <- CPackParser(tokens).right
    } yield parseTree
  }

  def getTokens(filename: String): Either[CCompilationError, List[CToken]] = {
    for {
      code <- Preprocessor(filename).right
      tokens <- CLexer(code).right
    } yield tokens
  }
}
