package tipsy.compiler

import scalaz._
import tipsy.lexer.CLexer
import tipsy.lexer.CToken._
import tipsy.parser._
import scala.sys.process._

object WorkflowCompiler {
  def apply(filename: String): \/[CCompilationError, ParseTree] = {
    for {
      code <- getCode(filename)
      tokens <- CLexer(code)
      parseTree <- CPackParser(tokens)
    } yield parseTree
  }

  def getCode(filename: String): \/[CPreError, String] = {
    val res = for {
      newfile <- Preprocessor.clangFormat(filename)
      code <- Preprocessor.gcc(newfile)
      _ <- \/-(s"rm -f ${filename}.1".!)
    } yield code
    res
  }

  def getTokens(filename: String): \/[CCompilationError, List[CToken]] = {
    for {
      code <- getCode(filename)
      tokens <- CLexer(code)
    } yield tokens
  }
}
