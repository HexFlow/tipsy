package tipsy.compare

import tipsy.parser._

case class Corrections(line: Int)

object Correct {
  type CorrError = String

  def simpleDecl[T](value: T) = Right(value).right

  def apply(t1: ParseTree, t2: ParseTree): Either[CorrError, Any] = {
    for {
      tli <- getTopListItems(t1, t2).right
      fxns <- (getFunctions _).tupled(tli).right // Passing tuple as args

      main1 <- getFxnByName(fxns._1, "main").right
      main2 <- getFxnByName(fxns._2, "main").right

      stats1 <- simpleDecl(ProgStats(main1)) // Convert normalfxn to either
      stats2 <- simpleDecl(ProgStats(main2))
    } yield fxns
  }

  private def getTopListItems(x1: ParseTree, x2: ParseTree) = {
    (x1, x2) match {
      case (TopList(tl1), TopList(tl2)) => Right((tl1, tl2))
      case _ => Left("Bad parse trees")
    }
  }

  private def getFunctions(i1: List[ParseTree], i2: List[ParseTree]) = {
    def getFxns(arg: List[ParseTree]) = {
      arg collect { case x @ Definition(_, _, _) => x }
    }

    Right((getFxns(i1), getFxns(i2)))
  }

  private def getFxnByName(tree: List[ParseTree], n: String) = {
    tree collect {
      case x @ FxnDefinition(n, _, Some(body)) if n.qt.name == "main" => body
    } headOption match {
      case Some(body) => Right(body)
      case None => Left("No main function found")
    }
  }
}
