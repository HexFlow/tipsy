package tipsy.compare

import tipsy.parser._

case class Corrections(line: Int)

object Correct {
  type CorrError = String

  def apply(t1: ParseTree, t2: ParseTree): Either[CorrError, Any] = {
    for {
      tli <- getTopListItems((t1, t2)).right
      fxns <- getFunctions(tli).right
    } yield fxns
  }

  private def getTopListItems(x: (ParseTree, ParseTree)) = {
    x match {
      case (TopList(tl1), TopList(tl2)) => Right((tl1, tl2))
      case _ => Left("Bad parse trees")
    }
  }

  private def getFunctions(x: (List[ParseTree], List[ParseTree])) = {
    def getFxns(arg: List[ParseTree]) = {
      arg collect { case x @ Definition(_, _, _) => x }
    }

    x match {
      case (i1, i2) => {
        Right((getFxns(i1), getFxns(i2)))
      }
    }
  }
}
