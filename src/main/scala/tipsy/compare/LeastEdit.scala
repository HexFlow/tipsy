package tipsy.compare

import tipsy.lexer._
import tipsy.parser._
import scalaz._

object LeastEdit {
  def apply(trees: List[ParseTree]) {
    trees.combinations(2).map {
      case Seq(t1, t2) => {
        println("Starting 2 new trees")
        val v1 = t1.compress.toVector
        val v2 = t2.compress.toVector
        println(editDist(v1, v2, v1.length, v2.length))
        println("Done")
      }
      case x => println(x)
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
