package tipsy.compare

import tipsy.parser._

import scala.math._

object Compare {
  val INF: Double = 1000000
  val deletedFxnPerLinePenalty = 20 // TODO: Fix this.
  val addedFxnPerLinePenalty = 25
  val fxnOrderingPenaltyScaling = 25
  val pairingUpPenaltyThreshold = 0.5

  def findDist(n1: NormCode, n2: NormCode): EditRet = {
    (n1, n2) match {
      case (NormCode(fxns1), NormCode(fxns2)) =>
        val fdiff = math.abs(fxns1.length - fxns2.length)
        val pairsToTry = pairUpFxnsList(fxns1, fxns2)
        pairsToTry.map { pairedResult =>
          val pairedDists = pairedResult.paired.map {
            case (f1, f2) => compareTwoFxns(f1, f2)
          }.reduceLeft(_ + _)
          val deletionCost = pairedResult.deleted.map(_.cf.length).sum * deletedFxnPerLinePenalty
          val additionCost = pairedResult.added.map(_.cf.length).sum * addedFxnPerLinePenalty
          val fxnOrderingCost = pairedResult.penalty * fxnOrderingPenaltyScaling

          pairedDists + deletionCost + additionCost + fxnOrderingCost
        }.foldLeft(EditRet(List(), INF)) {
          case (a, b) => if (a.dist <= b.dist) a else b
        }
    }
  }

  def compareTwoFxns(tree1: NormFxn, tree2: NormFxn): EditRet = {
    val cfEnum1 = tree1.cf.reverse
    val cfEnum2 = tree2.cf.reverse
    val l1 = cfEnum1.length
    val l2 = cfEnum2.length

    lazy val editDistTable: LazyVector[LazyVector[(EditRet, (Int, Int))]] =
      LazyVector.tabulate(l1 + 1, l2 + 1) { (x, y) => distance(x, y) }

    def distance(x: Int, y: Int): (EditRet, (Int, Int)) = {
      (x, y) match {
        case (0, 0) => (EditRet(List(), 0), (0, 0))
        case (i, 0) => go(i - 1, 0, Some(DEL_d))
        case (0, j) => go(0, j - 1, Some(ADD_d))
        case (i, j) => {
          if (cfEnum1(i - 1) == cfEnum2(j - 1)) go(i - 1, j - 1, None)
          else {
            val pen = Seq( go(i - 1, j, Some(DEL_d))
              , go(i, j - 1, Some(ADD_d))
              , go(i - 1, j - 1, Some(REPLACE_d))
            ).minBy(_._1.dist)

            val anchorFactor: Double = ((cfEnum1(i-1), cfEnum2(j-1)) match {
              case (BLOCKOPEN(), _) => 40
              case (_, BLOCKOPEN()) => 40
              case (BLOCKCLOSE(), _) => 40
              case (_, BLOCKCLOSE()) => 40
              case _ => 0
            })

            (pen._1 + anchorFactor, pen._2)
          }
        }
      }
    }

    def cost(i: Int, j: Int, action: DiffChange, param: Int): Double = {
      action match {
        case DEL_d => 20.0 // 2.0 + param * 5.0
        case ADD_d => 20.0 // 2.0 + param * 5.0
        case _     => {
          (cfEnum1(i), cfEnum2(j)) match {
            case (POSTEXPR(expr1), POSTEXPR(expr2)) => {
              val normFactor = max(expr1.length, expr2.length) * 2
              val ret = compareTwoExpr(expr1.toVector, expr2.toVector, param)
              (10.0 /*1.0 + 5.0 * param*/) * ret/normFactor
            }
            case _ => 10.0 // 1.0 + 5.0 * param
          }
        }
      }
    }

    def go(i: Int, j: Int, action: Option[DiffChange]): (EditRet, (Int, Int)) = {
      val (editRet, (b, c)) = editDistTable(i)(j)
      if (!action.isDefined) (editRet, (b, c))
      else action.get match {
             case DEL_d => (editRet.correct(Diff(DEL_d, None, Some(cfEnum1(i))), cost(i, j, DEL_d, 3 * b)), (b + 1, c))
             case ADD_d => (editRet.correct(Diff(ADD_d, Some(cfEnum2(j)), None), cost(i, j, ADD_d, 3 * b)), (b + 1, c))
             case _     => {
               (editRet.correct(Diff(REPLACE_d, Some(cfEnum2(j)), Some(cfEnum1(i))), cost(i, j, REPLACE_d, c)), (b, c + 1))
             }
           }
    }

    val res = editDistTable(l1)(l2)._1
    res.copy(diffs = res.diffs.reverse.map (_.copy(fxn = tree1.name)))
  }

  def compareTwoExpr(expr1: Vector[String], expr2: Vector[String], param: Int): Double = {
    val l1 = expr1.length
    val l2 = expr2.length

    lazy val editDistTable: LazyVector[LazyVector[Double]] =
      LazyVector.tabulate(l1 + 1, l2 + 1) { (x, y) => distance(x, y) }

      def distance(x: Int, y: Int): Double = {
        (x, y) match {
          case (0, 0) => 0.0
          case (i, 0) => go(i - 1, 0, 2*i)
          case (0, j) => go(0, j - 1, 2*j)
          case (i, j) => {
            if (expr1(i - 1) == expr2(j - 1)) go(i - 1, j - 1, 0)
            else Seq( go(i - 1, j,     2)
                    , go(i,     j - 1, 2)
                    , go(i - 1, j - 1, 1)
                    ).min
          }
        }
      }

      def go(i: Int, j: Int, cost: Double): Double = {
        val dist = editDistTable(i)(j)
        dist + cost
      }

      editDistTable(l1)(l2)
  }

  case class PairResult(
    paired: List[(NormFxn, NormFxn)],
    added: List[NormFxn],
    deleted: List[NormFxn],
    penalty: Double // Penalty is 0 for functions which are surely corresponding.
  )

  // Takes a list of normalized functions. Provides a list of PairResult, which
  // basically contains paired up functions, as well as a penalty of the permutation.
  // That means, that a pairing-up in which functions were in their original
  // usage order, has a 0 penalty.
  // It also contains functions which were not paired up.
  // Note: The first list is considered as the buggy program which needs corrections.
  // So the 'added' list in PairResult is the list of functions in f2 which were
  // not paired with a function in f1, in that PairResult.
  def pairUpFxnsList(f1: List[NormFxn], f2: List[NormFxn]): List[PairResult] = {
    val l1 = f1.length
    val l2 = f2.length
    f2.zipWithIndex.permutations.map { permutedFxnsWithOrigIndex =>
      val fxns = permutedFxnsWithOrigIndex.map(_._1)
      val paired = f1.zip(fxns)
      val added = f1.drop(l2)
      val deleted = fxns.drop(l1)

      // Every function that is not in its original place gets a penalty.
      // The penalty is out of 1.
      val penalty = (permutedFxnsWithOrigIndex.map(_._2).zipWithIndex.map {
        case (i1, i2) => if (i1 == i2) 0 else 1
      }.sum).toDouble / fxns.length.toDouble

      PairResult(
        paired,
        added,
        deleted,
        penalty
      )
    }.toList.filter(_.penalty < pairingUpPenaltyThreshold)
  }

  // Takes functions of 2 programs, and pairs them up based on similarity.
  // See docstring of this package for more details.
  // private def getMatchingFxns(f1: List[FxnDefinition], f2: List[FxnDefinition])
  //     : List[(FxnDefinition, FxnDefinition)] = {

  //   def strComp(a: String, b: String): Int = {
  //     val dist = LeastEdit.levenshteinDist(a, b)
  //       (dist / a.length * 100).toInt
  //   }

  //   // Convert List[FxnDefinition] to List[(FxnDefinition, Int)]
  //   val userFxns = f1.zipWithIndex
  //   val compFxns = f2.zipWithIndex

  //   // Convert List[FxnDefinition] to List[(FxnDefinition, FxnDefinition)]
  //   userFxns.map {
  //     case ufunc @ (FxnDefinition(TypedIdent(_, IDENT(uname)), _, _), upos) =>

  //       // Find a similar function
  //       val simfunc: Option[FxnDefinition] = compFxns.map {

  //         // Calculate score for each function to be compared
  //         case newfunc @ (FxnDefinition(TypedIdent(_, IDENT(nname)), _, _), npos) =>
  //           val nameDist = strComp(uname, nname)
  //           val posDist  = math.abs(upos - npos) * 30
  //           (newfunc._1, nameDist + posDist)

  //       }.reduceOption { (e1, e2) =>

  //         // Select the one with min score
  //         if (e1._2 > e2._2) e2
  //         else e1
  //       }.map(_._1)

  //       (ufunc._1, simfunc)
  //   }.collect {
  //     // Remove functions which did not have similar fxn
  //     // This will remove those of type (f, None)
  //     // Converts List[(FxnDefinition, Option[FxnDefinition])] to
  //     // List[(FxnDefinition, FxnDefinition)] with None elements dropped
  //     //
  //     // Reason: list.collect takes as argument a partial function.
  //     // Such functions are only defined on certain inputs.
  //     // All other functions are dropped.
  //     case x @ (f, Some(g)) => (f, g)
  //   }
  // }

}
