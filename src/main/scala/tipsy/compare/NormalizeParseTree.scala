package tipsy.compare

import tipsy.parser._
import tipsy.lexer._
import tipsy.frontend._
import tipsy.compiler._

object NormalizeParseTree {

  // We assume pt contains functions sorted in use order.
  def apply(parseTree: ParseTree): Either[CCompilationError, List[(String, List[CFEnum])]] = {
    for {

      topList <- (parseTree match {
        case TopList(tl) => Right(tl)
        case _ => Left(CCustomError("Given tree was not a TopList"))
      }).right

    } yield getFunctions(topList).map(fxnToPair(_))
  }

  private def fxnToPair(x: FxnDefinition): (String, List[CFEnum]) = {
    x match {
      case FxnDefinition(TypedIdent(_, IDENT(name)), _, _) =>
        (name, FlowGraphTweaks.renameFxnNames(x.compress))
    }
  }

  // Helper to return list of functions
  private def getFunctions(i1: List[ParseTree]): List[FxnDefinition] = {
    def getFxns(arg: List[ParseTree]): List[FxnDefinition] = {
      arg collect { case x @ FxnDefinition(_, _, _) => x }
    }
    getFxns(i1)
  }
}
