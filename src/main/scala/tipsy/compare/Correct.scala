package tipsy.compare

import tipsy.parser._
import tipsy.lexer._
import tipsy.frontend._

case class Corrections(line: Int)

object Correct {
  type CorrError = String

  // Helper to allow simple statements inside for...yield block
  def simpleDecl[T](value: T) = Right(value).right

  def apply(t1: ParseTree, t2: ParseTree)
      : Either[CorrError, List[(String, EditRet)]] = {

    def getNamesHelper(k: List[(FxnDefinition, FxnDefinition)]) = {
      k.map {
        case (f1 @ FxnDefinition(TypedIdent(_, IDENT(name)), _, _), f2) =>
          (name, f1, f2)
      }
    }

    for {
      tli <- getTopListItems(t1, t2).right
      fxns <- (getFunctions _).tupled(tli).right // Passing tuple as args

      // stats1 <- simpleDecl(ProgStats(main1)) // Convert normalfxn to either
      // stats2 <- simpleDecl(ProgStats(main2))

      similarFxnList <- simpleDecl(getMatchingFxns(fxns._1, fxns._2))
      similarFxnsWithNames <- simpleDecl(getNamesHelper(similarFxnList))

    } yield similarFxnsWithNames.map(x =>
      (x._1, LeastEdit.compareTwoTrees(x._2, x._3)))
  }

  // Helper to extract the TopList element from a program's parsetree
  private def getTopListItems(x1: ParseTree, x2: ParseTree) = {
    (x1, x2) match {
      case (TopList(tl1), TopList(tl2)) => Right((tl1, tl2))
      case _ => Left("Bad parse trees")
    }
  }

  // Helper to return list of functions from two programs
  private def getFunctions(i1: List[ParseTree], i2: List[ParseTree]) = {
    def getFxns(arg: List[ParseTree]) = {
      arg collect { case x @ FxnDefinition(_, _, _) => x }
    }

    Right((getFxns(i1), getFxns(i2)))
  }

  // Helper to convert Options to Eithers with added error messages
  private def optToEither[T, E](opt: Option[T], err: E): Either[E, T] = {
    opt match {
      case Some(x) => Right(x)
      case _ => Left(err)
    }
  }

  /**
    * Takes functions of 2 programs, and pairs them up.
    * Criteria used:
    * 1. Similarity of name.
    * 2. Order of use in program.
    *
    * It is possible for some functions to not have any similar function,
    * or two functions to have the same similar function
    */
  private def getMatchingFxns(f1: List[FxnDefinition], f2: List[FxnDefinition])
      : List[(FxnDefinition, FxnDefinition)] = {

    def strComp(a: String, b: String): Int = {
      val dist = LeastEdit.levenshteinDist(a, b)
        (dist / a.length * 100).toInt
    }

    // Convert List[FxnDefinition] to List[(FxnDefinition, Int)]
    val userFxns = f1.zipWithIndex
    val compFxns = f2.zipWithIndex

    // Convert List[FxnDefinition] to List[(FxnDefinition, FxnDefinition)]
    userFxns.map {
      case ufunc @ (FxnDefinition(TypedIdent(_, IDENT(uname)), _, _), upos) =>

        // Find a similar function
        val simfunc: Option[FxnDefinition] = compFxns.map {

          // Calculate score for each function to be compared
          case newfunc @ (FxnDefinition(TypedIdent(_, IDENT(nname)), _, _), npos) =>
            val nameDist = strComp(uname, nname)
            val posDist  = math.abs(upos - npos) * 30
            (newfunc._1, nameDist + posDist)

        }.reduceOption { (e1, e2) =>

          // Select the one with min score
          if (e1._2 > e2._2) e2
          else e1
        }.map(_._1)

        (ufunc._1, simfunc)
    }.collect {
      // Remove functions which did not have similar fxn
      // This will remove those of type (f, None)
      // Converts List[(FxnDefinition, Option[FxnDefinition])] to
      // List[(FxnDefinition, FxnDefinition)] with None elements dropped
      case x @ (f, Some(g)) => (f, g)
    }
  }

}
