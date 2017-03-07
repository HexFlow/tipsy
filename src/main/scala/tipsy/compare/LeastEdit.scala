package tipsy.compare

import tipsy.lexer._
import tipsy.parser._
import scalaz._

object LeastEdit {
  def apply(trees: List[ParseTree]): List[(Int, Int, Double)] = {
    if (trees.length < 2) {
      println("[Warning] Least Edit mode requires at least 2 trees")
    }
    trees.zipWithIndex.combinations(2).map {
      case Seq(t1, t2) => {
        println("Starting 2 new trees " + t1._2 + " " + t2._2)
        val v1 = t1._1.compress.toVector
        val v2 = t2._1.compress.toVector
        val k = (editDist(v1, v2, v1.length, v2.length))
        println(k)
        (t1._2, t2._2, 1/(k.toDouble+0.1))
      }
      case _ => ???
    }.toList
  }

  def editDistRecur(
    s1: Vector[CFEnum], s2: Vector[CFEnum], m: Int, n: Int
  ): Int = {
    if (m == 0) n
    else if (n == 0) m
    else if (s1(m-1) == s2(n-1))
      editDist(s1, s2, m-1, n-1)
    else
      1 + List(
        editDist(s1, s2, m, n-1),
        editDist(s1, s2, m-1, n),
        editDist(s1, s2, m-1, n-1)
      ).reduceLeft(_ min _)
  }

  val editDist = Memo.mutableHashMapMemo { (editDistRecur _).tupled }
}
