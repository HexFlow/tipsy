package tipsy.compiler

import tipsy.lexer.CLexer
import tipsy.lexer.CToken._
import tipsy.parser._
import scala.sys.process._

object WorkflowCompiler {
  def apply(filename: String): Either[CCompilationError, ParseTree] = {
    for {
      code <- getCode(filename)
      tokens <- CLexer(code).right
      parseTree <- CPackParser(tokens).right
    } yield parseTree
  }

  def getCode(filename: String): Either[CPreError, String] = {
    val res = for {
      newfile <- Preprocessor.clangFormat(filename).right
      code <- Preprocessor.gcc(newfile).right
      _ <- Right(s"rm -f ${filename}.1".!).right
    } yield code
    res
  }

  def getTokens(filename: String): Either[CCompilationError, List[CToken]] = {
    for {
      code <- getCode(filename)
      tokens <- CLexer(code).right
    } yield tokens
  }
}
