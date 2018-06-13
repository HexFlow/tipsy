package tipsy.compare

import scalaz._
import tipsy.parser._
import tipsy.lexer.CToken._
import tipsy.compiler._

case class NormFxn(name: String, cf: List[CFEnum]) {
  def length(): Int = {
    cf.length
  }
}

case class NormCode(fxns: List[NormFxn]) {
  def length(): Int = {
    fxns.map(_.length).sum
  }
}

object NormalizeParseTree {

  // We assume pt contains functions sorted in use order.
  def apply(parseTree: ParseTree): \/[CCompilationError, NormCode] = {
    for {
      topList <- (parseTree match {
        case TopList(tl) => \/-(tl)
        case _ => -\/(CCustomError("Given tree was not a TopList"))
      })
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
