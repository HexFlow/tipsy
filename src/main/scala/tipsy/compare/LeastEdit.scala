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
  def apply(trees: List[ParseTree]): List[(Int, Int, Double)] = {
    if (trees.length < 2) {
      println("[Warning] Least Edit mode requires at least 2 trees")
    }
    trees.zipWithIndex.combinations(2).map {
      case Seq(t1, t2) => {
        println("Starting 2 new trees " + t1._2 + " " + t2._2)
        val k = compareTwoTrees(t1._1, t2._1)
        println(k)
        (t1._2, t2._2, 1/(k.dist.toDouble+0.1))
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
    val v1 = FlowGraphTweaks(tree1.compress).toVector
    val v2 = FlowGraphTweaks(tree2.compress).toVector
    editDist(v1, v2, v1.length, v2.length)
  }

  def editDistRecurExpr( s1: Vector[String], s2: Vector[String], m: Int, n: Int): Int = {
    if (m == 0) n*20
    else if (n == 0) m*20
    else if (s1(m-1) == s2(n-1))
      editDistExpr(s1, s2, m-1, n-1)
    else {
      val ret = 20 + List(
        editDistExpr(s1, s2, m, n-1),
        editDistExpr(s1, s2, m-1, n),
        editDistExpr(s1, s2, m-1, n-1)
      ).reduceLeft(_ min _)
      ret
    }
  }

  def editDistRecur(s1: Vector[CFEnum], s2: Vector[CFEnum],
    m: Int, n: Int): EditRet = {

    if (m == 0) EditRet(List(), n*20)

    else if (n == 0) EditRet(s1.map(Diff(DEL_d, _)).toList, m*20)

    else if (s1(m-1) == s2(n-1)) editDist(s1, s2, m-1, n-1)

    else {
      var d = 100000000

      (s1(m-1), s2(n-1)) match {
        case (POSTEXPR(y), POSTEXPR(z)) => {
          val p = editDistExpr((y.toVector, z.toVector, y.length, z.length))
          val c = p / Math.max(y.length, z.length)
          d = c
        }
        case _ =>
      }

      val opt1 = editDist((s1, s2, m, n-1))
        .correct(Diff(ADD_d, s2(n-1)), 20)

      val opt2 = editDist((s1, s2, m-1, n))
        .correct(Diff(DEL_d, s1(m-1)), 20)

      // val opt3 = editDist((s1, s2, m-1, n-1))
      //   .correct(Diff(ADD_d, s2(n-1)), 20)
      //   .correct(Diff(DEL_d, s1(m-1)), 20)

      val opt4 = editDist((s1, s2, m-1, n-1))
        .correct(Diff(ADD_d, s2(n-1)), d/2)
        .correct(Diff(DEL_d, s1(m-1)), d/2)

      val ret: EditRet = List(
        opt1, opt2, opt4
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

  val editDist = Memo.mutableHashMapMemo { (editDistRecur _).tupled }
  val editDistExpr = Memo.mutableHashMapMemo { (editDistRecurExpr _).tupled }

  def levenshteinDist[A](a: Iterable[A], b: Iterable[A]) =
    ((0 to b.size).toList /: a) { (prev, x) =>
      (prev zip prev.tail zip b).scanLeft(prev.head + 1) {
        case (h, ((d, v), y)) =>
          min(min(h + 1, v + 1), d + (if (x == y) 0 else 1))
      }
    } last
}
