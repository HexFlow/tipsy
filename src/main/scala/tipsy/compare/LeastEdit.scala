package tipsy.compare

import tipsy.parser._

import scala.math._

sealed trait DiffChange
case object ADD_d extends DiffChange
case object DEL_d extends DiffChange
case object REPLACE_d extends DiffChange

case class Diff (
  change: DiffChange,
  addEntry: Option[CFEnum],
  delEntry: Option[CFEnum])

case class EditRet (diffs: List[Diff], dist: Double) {
  def correct(d: Diff, dis: Double): EditRet = {
    this.copy(diffs = diffs ++ List(d), dist = dis)
  }
  def /(v: Double) = {
    this match {
      case EditRet(x, y) => {
        EditRet(x, y/v)
      }
    }
  }
  def apply(a: EditRet) {
    this.copy(diffs = a.diffs, dist = a.dist)
  }
}

object LeastEdit {
  def apply(trees: List[ParseTree], cluster: Boolean): List[(Int, Int, Double)] = {
    if (trees.length < 2) {
      println("[Warning] Least Edit mode requires at least 2 trees")
    }
    trees.zipWithIndex.combinations(2).map {
      case Seq(t1, t2) => {
        println("Starting 2 new trees " + t1._2 + " " + t2._2)
        val k = compareTwoTrees(t1._1, t2._1)
/*        val mb = 1024 * 1024
 *        println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
 *        println("** Free Memory:  " + (runtime.freeMemory / mb))
 *        println("** Total Memory: " + (runtime.totalMemory) / mb)
 *        println("** Max Memory:   " + (runtime.maxMemory) / mb)
 */
        cluster match {
          case true => (t1._2, t2._2, k.dist)
          case false => (t1._2, t2._2, 10/(k.dist+0.1))
        }
      }
      case _ => ???
    }.toList
  }

  var editDist: Array[Array[(EditRet, (Int, Int))]] = Array[Array[(EditRet, (Int, Int))]]()
  var editDistExpr: Array[Array[(Double, (Int, Int))]] = Array[Array[(Double, (Int, Int))]]()
  var cfenum1: Vector[CFEnum] = Vector[CFEnum]()
  var cfenum2: Vector[CFEnum] = Vector[CFEnum]()
  var string1: Vector[String] = Vector[String]()
  var string2: Vector[String] = Vector[String]()
  val powersOfE: Array[Double] = PowersOfN(math.exp(0.0555), 300)

  def compareWithTrees(prog: ParseTree, trees: List[ParseTree]): List[(ParseTree, Double)] = {
    trees.map { case tree =>
      val k = compareTwoTrees(prog, tree)
      (tree, 1/(k.dist+0.1))
    }.toList
  }

  def compareTwoTrees(tree1: ParseTree, tree2: ParseTree): EditRet = {
    cfenum1 = FlowGraphTweaks(tree1.compress).toVector
    cfenum2 = FlowGraphTweaks(tree2.compress).toVector
    val l1 = cfenum1.length
    val l2 = cfenum2.length
    println(l1 + 1, l2 + 1)
    editDist = Array.fill(l1 + 1)(Array.fill(l2 + 1)((EditRet(List(), -1), (0, 0))))
    editDistRecur(l1, l2)
    editDist(l1)(l2)._1
  }

  private def distance(c: Int): Double = {
    val k = c / 7
    val left = c - 7*k
    var ret = 0.0
    for (i <- 1 to (k min 3000)) {
      ret += 7.0 * powersOfE(i)
    }
    ret += left.toDouble * powersOfE(k + 1 min 3000)
    if (ret == Double.PositiveInfinity) {
      println("** [error] Positive Infinity was reached :(")
    }
    ret
  }

  def editDistRecurExpr(m: Int, n: Int): Unit = {
    editDistExpr(0)(0) = (0.0, (0, 0))
    for (i <- 1 to m) {
      val (a, (b, c)) = editDistExpr(i - 1)(0)
      editDistExpr(i)(0) = (a + 20, (b + 1, c))
    }
    for (j <- 1 to n) {
      val (a, (b, c)) = editDistExpr(0)(j - 1)
      editDistExpr(0)(j) = (a + 20, (b + 1, c))
    }
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        if (string1(i - 1) == string2(j - 1)) {
          editDistExpr(i)(j) = editDistExpr(i - 1)(j - 1)
        } else {
          val a: Array[Double] = Array(-1.0, editDistExpr(i - 1)(j)._1, editDistExpr(i)(j - 1)._1, editDistExpr(i - 1)(j - 1)._1)
          val b: Array[Int] = Array(-1, editDistExpr(i - 1)(j)._2._1, editDistExpr(i)(j - 1)._2._1, editDistExpr(i - 1)(j - 1)._2._1)
          val c: Array[Int] = Array(-1, editDistExpr(i - 1)(j)._2._2, editDistExpr(i)(j - 1)._2._2, editDistExpr(i - 1)(j - 1)._2._2)
          val penalty: Array[Double] = Array(-1.0, a(1) + 20, a(2) + 20, a(3) + 20)
          val eps = 1e-6
          var ind = 1

          if (penalty(1) < penalty(2) - eps) {
            ind = 1
          } else if (penalty(1) <= penalty(2) + eps && penalty(1) >= penalty(2) - eps) {
            if (b(1) < b(2)) {
              ind = 1
            } else if (b(1) == b(2)) {
              if (c(1) < c(2)) {
                ind = 1
              } else {
                ind = 2
              }
            } else {
              ind = 2
            }
          } else {
            ind = 2
          }

          if (penalty(ind) < penalty(3) - eps) {
            ind = ind
          } else if (penalty(ind) <= penalty(3) + eps && penalty(ind) >= penalty(3) - eps) {
            if (b(ind) < b(3)) {
              ind = ind
            } else if (b(ind) == b(3)) {
              if (c(ind) < c(3)) {
                ind = ind
              } else {
                ind = 3
              }
            } else {
              ind = 3
            }
          } else {
            ind = 3
          }

          if (ind == 1) {
            editDistExpr(i)(j) = (penalty(1), (b(1) + 1, c(1)))
          } else if (ind == 2) {
            editDistExpr(i)(j) = (penalty(2), (b(2) + 1, c(2)))
          } else {
            editDistExpr(i)(j) = (penalty(3), (b(3) + 1, c(3)))
          }
        }
      }
    }
  }

  def editDistRecur(m: Int, n: Int): Unit= {
    editDist(0)(0) = (EditRet(List(), 1), (0, 0))
    for (i <- 1 to m) {
      val (a, (b, c)) = editDist(i - 1)(0)
      editDist(i)(0) = (a.correct(Diff(DEL_d, None, Some(cfenum1(i - 1))), (a.dist + 20) min (1<<27).toDouble), (b + 1, c))
    }
    for (j <- 1 to n) {
      val (a, (b, c)) = editDist(0)(j - 1)
      editDist(0)(j) = (a.correct(Diff(ADD_d, Some(cfenum2(j - 1)), None), (a.dist + 20) min (1<<27).toDouble), (b + 1, c))
    }
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        if (cfenum1(i - 1) == cfenum2(j - 1)) {
          editDist(i)(j) = editDist(i - 1)(j - 1)
        } else {
          val a: Array[EditRet] = Array(EditRet(List(), -1.0), editDist(i - 1)(j)._1, editDist(i)(j - 1)._1, editDist(i - 1)(j - 1)._1)
          val b: Array[Int] = Array(-1, editDist(i - 1)(j)._2._1, editDist(i)(j - 1)._2._1, editDist(i - 1)(j - 1)._2._1)
          val c: Array[Int] = Array(-1, editDist(i - 1)(j)._2._2, editDist(i)(j - 1)._2._2, editDist(i - 1)(j - 1)._2._2)
          val penalty: Array[Double] = Array(-1.0,
                                             (a(1).dist + 20) min (1<<27).toDouble,
                                             (a(2).dist + 20) min (1<<27).toDouble,
                                             (a(3).dist + 20) min (1<<27).toDouble)
          val eps = 1e-6
          var ind = 1

          var d = 0.0

          (cfenum1(i - 1), cfenum2(j - 1)) match {
            case (POSTEXPR(y), POSTEXPR(z)) => {
              string1 = y.toVector
              string2 = z.toVector
              val (l1, l2) = (y.length, z.length)
              editDistExpr = Array.fill(l1 + 1)(Array.fill(l2 + 1)(-1.0, (0, 0)))
              editDistRecurExpr(l1, l2)
              d = editDistExpr(l1)(l2)._1 / Math.max(y.length, z.length).toDouble
            }
            case _ =>
          }

          penalty(3) = (penalty(3) max (a(3).dist + d)) min (1<<27).toDouble

          if (penalty(1) < penalty(2) - eps) {
            ind = 1
          } else if (penalty(1) <= penalty(2) + eps && penalty(1) >= penalty(2) - eps) {
            if (b(1) < b(2)) {
              ind = 1
            } else if (b(1) == b(2)) {
              if (c(1) < c(2)) {
                ind = 1
              } else {
                ind = 2
              }
            } else {
              ind = 2
            }
          } else {
            ind = 2
          }

          if (penalty(ind) < penalty(3) - eps) {
            ind = ind
          } else if (penalty(ind) <= penalty(3) + eps && penalty(ind) >= penalty(3) - eps) {
            if (b(ind) < b(3)) {
              ind = ind
            } else if (b(ind) == b(3)) {
              if (c(ind) < c(3)) {
                ind = ind
              } else {
                ind = 3
              }
            } else {
              ind = 3
            }
          } else {
            ind = 3
          }

          if (ind == 1) {
            editDist(i)(j) = (a(1).correct(Diff(DEL_d, None, Some(cfenum1(i - 1))), penalty(1)), (b(1) + 1, c(1)))
          } else if (ind == 2) {
            editDist(i)(j) = (a(2).correct(Diff(ADD_d, Some(cfenum2(j - 1)), None), penalty(2)), (b(2) + 1, c(2)))
          } else {
            editDist(i)(j) = (a(3).correct(Diff(REPLACE_d, Some(cfenum2(j - 1)), Some(cfenum1(i - 1))), penalty(3)), (b(3) + 1, c(3) + 1))
          }

        }
      }
    }
  }

  def levenshteinDist[A](a: Iterable[A], b: Iterable[A]) = {
    ((0 to b.size).toList /: a) { (prev, x) =>
      (prev zip prev.tail zip b).scanLeft(prev.head + 1) {
        case (h, ((d, v), y)) =>
          min(min(h + 1, v + 1), d + (if (x == y) 0 else 1))
      }
    } last
  }
}
