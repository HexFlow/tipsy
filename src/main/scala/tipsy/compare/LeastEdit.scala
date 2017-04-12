package tipsy.compare

import tipsy.parser._
import scalaz._

sealed trait DiffChange
case object ADD_d extends DiffChange
case object DEL_d extends DiffChange

case class Diff (
  change: DiffChange,
  entry: CFEnum
)

case class EditRet (
  diffs: List[Diff],
  dist: Int
) {
  def correct(d: Diff, dis: Int): EditRet = {
    this.copy(diffs = d :: diffs, dist = dis + dist)
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
        val v1 = FlowGraphTweaks(t1._1.compress).toVector
        val v2 = FlowGraphTweaks(t2._1.compress).toVector
        val k = (editDist(v1, v2, v1.length, v2.length))
        println(k)
        (t1._2, t2._2, 1/(k.dist.toDouble+0.1))
      }
      case _ => ???
    }.toList
  }

  def compareWithTrees(
    prog: ParseTree,
    trees: List[ParseTree]
  ): List[(ParseTree, Double)] = {

    trees.map { case tree =>
      val v1 = FlowGraphTweaks(prog.compress).toVector
      val v2 = FlowGraphTweaks(tree.compress).toVector
      val k = (editDist(v1, v2, v1.length, v2.length))
      (tree, 1/(k.dist.toDouble+0.1))
    }.toList
  }

  def editDistRecur(
    s1: Vector[CFEnum], s2: Vector[CFEnum], m: Int, n: Int
  ): EditRet = {
    if (m == 0) (???, n*20)
    else if (n == 0) (???, m*20)
    else if (s1(m-1) == s2(n-1))
      editDist(s1, s2, m-1, n-1)
    else {
      var d = 100000000

      (s1(m-1), s2(n-1)) match {
        case (POSTEXPR(y), POSTEXPR(z)) => {
          d = editDist(y.toVector, z.toVector,
            y.length, z.length)/Math.max(y.length, z.length)
        }
        case _ =>
      }

      val opt1 = editDist(s1, s2, m, n-1)
        .correct(Diff(ADD_d, s2(n-1)), 20)

      val opt2 = editDist(s1, s2, m-1, n)
        .correct(Diff(DEL_d, s1(m-1)), 20)

      val opt3 = editDist(s1, s2, m-1, n-1)
        .correct(Diff(ADD_d, s2(n-1)), 20)
        .correct(Diff(DEL_d, s1(m-1)), 20)

      val opt4 = editDist(s1, s2, m-1, n-1)
        .correct(Diff(ADD_d, s2(n-1)), d/2)
        .correct(Diff(DEL_d, s1(m-1)), d/2)

      val ret = List(
        opt1,
        opt2,
        opt3,
        opt4
      ).reduceLeft(_.dist min _.dist)

      //if(m!=0 && n!=0)
      //println("Now comparing " + s1(m-1) + " " + s2(n-1))
      //println("Reuturning: " + ret)
      ret
    }
  }

  val editDist = Memo.mutableHashMapMemo { (editDistRecur _).tupled }
}
