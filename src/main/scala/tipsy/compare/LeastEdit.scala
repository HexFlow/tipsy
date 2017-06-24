package tipsy.compare

import tipsy.parser._
import scalaz._

import scala.math.min

sealed trait DiffChange
case object ADD_d extends DiffChange
case object DEL_d extends DiffChange

case class Diff (
  change: DiffChange,
  entry: CFEnum
)

case class EditRet (diffs: List[Diff], dist: Int) {
  def correct(d: Diff, dis: Int): EditRet = {
    this.copy(diffs = diffs ++ List(d), dist = dis + dist)
  }

  def /(v: Int) = {
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
  var editDist: Array[Array[EditRet]] = Array[Array[EditRet]]()
  var editDistExpr: Array[Array[Int]] = Array[Array[Int]]()
  var cfenum1: Vector[CFEnum] = Vector[CFEnum]()
  var cfenum2: Vector[CFEnum] = Vector[CFEnum]()
  var string1: Vector[String] = Vector[String]()
  var string2: Vector[String] = Vector[String]()

  def apply(trees: List[ParseTree], cluster: Boolean): List[(Int, Int, Double)] = {
    if (trees.length < 2) {
      println("[Warning] Least Edit mode requires at least 2 trees")
    }
    trees.zipWithIndex.combinations(2).map {
      case Seq(t1, t2) => {
        println("Starting 2 new trees " + t1._2 + " " + t2._2)
        val k = compareTwoTrees(t1._1, t2._1)
//        println(k)
/*        val mb = 1024 * 1024
        val runtime = Runtime.getRuntime
		println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
		println("** Free Memory:  " + runtime.freeMemory / mb)
		println("** Total Memory: " + runtime.totalMemory / mb)
		println("** Max Memory:   " + runtime.maxMemory / mb)*/
        cluster match {
          case true => (t1._2, t2._2, k.dist.toDouble)
          case false => (t1._2, t2._2, 1/(k.dist.toDouble  + 0.1))
        }
      }
      case _ => ???
    }.toList
  }

  def compareWithTrees(prog: ParseTree, trees: List[ParseTree]): List[(ParseTree, Double)] = {
    trees.map { case tree =>
      val k = compareTwoTrees(prog, tree)
      (tree, 1/(k.dist.toDouble+0.1))
    }.toList
  }

  def compareTwoTrees(tree1: ParseTree, tree2: ParseTree): EditRet = {
    cfenum1 = FlowGraphTweaks(tree1.compress).toVector
    cfenum2 = FlowGraphTweaks(tree2.compress).toVector
    editDist = Array.fill(cfenum1.length + 1)(Array.fill(cfenum2.length + 1)(EditRet(List(), -1)))
    editDistRecur(cfenum1.length, cfenum2.length)
  }

  def editDistRecurExpr(m: Int, n: Int): Int = {
    if (m == 0) n*20
    else if (n == 0) m*20
    else if (string1(m-1) == string2(n-1)) {
      if (editDistExpr(m-1)(n-1) == -1) {
        editDistExpr(m-1)(n-1) = editDistRecurExpr(m-1, n-1)
        editDistExpr(m-1)(n-1)
      } else {
        editDistExpr(m-1)(n-1)
      }
    }
    else {
      val opt1 = if (editDistExpr(m)(n-1) == -1) {
        editDistExpr(m)(n-1) = editDistRecurExpr(m, n-1)
        editDistExpr(m)(n-1)
      } else {
        editDistExpr(m)(n-1)
      }

      val opt2 = if (editDistExpr(m-1)(n) == -1) {
        editDistExpr(m-1)(n) = editDistRecurExpr(m-1, n)
        editDistExpr(m-1)(n)
      } else {
        editDistExpr(m-1)(n)
      }

      val opt3 = if (editDistExpr(m-1)(n-1) == -1) {
        editDistExpr(m-1)(n-1) = editDistRecurExpr(m-1, n-1)
        editDistExpr(m-1)(n-1)
      } else {
        editDistExpr(m-1)(n-1)
      }

      val ret = 20 + List(
        opt1, opt2, opt3
      ).reduceLeft(_ min _)
      ret
    }
  }

  def editDistRecur(m: Int, n: Int): EditRet = {

    if (m == 0) EditRet(List(), n*20)

    else if (n == 0) EditRet(cfenum1.map(Diff(DEL_d, _)).toList, m*20)

    else if (cfenum1(m-1) == cfenum2(n-1)) {
      if (editDist(m-1)(n-1) == EditRet(List(), -1)) {
        editDist(m-1)(n-1) = editDistRecur(m-1, n-1)
        editDist(m-1)(n-1)
      } else {
        editDist(m-1)(n-1)
      }
    }

    else {
      var d = 100000000

      (cfenum1(m-1), cfenum2(n-1)) match {
        case (POSTEXPR(y), POSTEXPR(z)) => {
          string1 = y.toVector
          string2 = z.toVector
          editDistExpr = Array.fill(y.length + 1)(Array.fill(z.length + 1)(-1))
          val p = editDistRecurExpr(y.length, z.length)
          val c = p / Math.max(y.length, z.length)
          d = c
        }
        case _ =>
      }

      val tOpt1: EditRet = if (editDist(m)(n-1) == EditRet(List(), -1)) {
        editDist(m)(n-1) = editDistRecur(m, n-1)
        editDist(m)(n-1)
      } else {
        editDist(m)(n-1)
      }
      val opt1 = tOpt1.correct(Diff(ADD_d, cfenum2(n-1)), 20)

      val tOpt2: EditRet = if (editDist(m-1)(n) == EditRet(List(), -1)) {
        editDist(m-1)(n) = editDistRecur(m-1, n)
        editDist(m-1)(n)
      } else {
        editDist(m-1)(n)
      }
      val opt2 = tOpt2.correct(Diff(DEL_d, cfenum1(m-1)), 20)

      val tOpt3: EditRet = if (editDist(m-1)(n-1) == EditRet(List(), -1)) {
        editDist(m-1)(n-1) = editDistRecur(m-1, n-1)
        editDist(m-1)(n-1)
      } else {
        editDist(m-1)(n-1)
      }
      val opt3 = tOpt3.correct(Diff(ADD_d, cfenum2(n-1)), d/2)
                      .correct(Diff(DEL_d, cfenum1(m-1)), d/2)

      val ret: EditRet = List(
        opt1, opt2, opt3
      ).reduceLeft( (a:EditRet, b:EditRet) => {
        (a,b) match {
          case (EditRet(a,b), EditRet(c,d)) => {
            if(b<d)
              EditRet(a,b)
            else
              EditRet(c,d)
          }
          case _ => EditRet(List(), 100000000)
        }
      })
      ret
    }
  }

  def levenshteinDist[A](a: Iterable[A], b: Iterable[A]) =
    ((0 to b.size).toList /: a) { (prev, x) =>
      (prev zip prev.tail zip b).scanLeft(prev.head + 1) {
        case (h, ((d, v), y)) =>
          min(min(h + 1, v + 1), d + (if (x == y) 0 else 1))
      }
    } last
}
