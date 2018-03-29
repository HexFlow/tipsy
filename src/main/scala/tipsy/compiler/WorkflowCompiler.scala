package tipsy.compiler

import tipsy.lexer.CLexer
import tipsy.lexer.CToken._
import tipsy.parser._

object WorkflowCompiler {
  def apply(filename: String): Either[CCompilationError, ParseTree] = {
    for {
      code <- Preprocessor(filename).right
      tokens <- CLexer(code).right
      parseTree <- CPackParser(tokens).right
    } yield parseTree
  }

  def getCode(filename: String): Either[CPreError, String] = {
    for {
      code <- Preprocessor(filename).right
    } yield code
  }

  def getTokens(filename: String): Either[CCompilationError, List[CToken]] = {
    for {
      code <- Preprocessor(filename).right
      tokens <- CLexer(code).right
    } yield tokens
  }
}
