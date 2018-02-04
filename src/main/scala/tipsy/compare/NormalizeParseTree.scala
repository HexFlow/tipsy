package tipsy.compare

import tipsy.parser._
import tipsy.lexer._
import tipsy.frontend._
import tipsy.compiler._

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

case class NormFxn(name: String, cf: List[CFEnum])

case class NormCode(fxns: List[NormFxn])

object NormalizeParseTree {

  // We assume pt contains functions sorted in use order.
  def apply(parseTree: ParseTree): Either[CCompilationError, NormCode] = {
    for {

      topList <- (parseTree match {
        case TopList(tl) => Right(tl)
        case _ => Left(CCustomError("Given tree was not a TopList"))
      }).right

    } yield NormCode(getFunctions(topList).map(fxnToPair(_)))
  }

  private def fxnToPair(x: FxnDefinition): NormFxn = {
    x match {
      case FxnDefinition(TypedIdent(_, IDENT(name)), _, _) =>
        NormFxn(name, FlowGraphTweaks.renameFxnNames(x.compress))
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
