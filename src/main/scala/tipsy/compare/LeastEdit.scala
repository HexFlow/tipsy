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
        val v1 = FlowGraphTweaks(t1._1.compress).toVector
        val v2 = FlowGraphTweaks(t2._1.compress).toVector
        val k = (editDist(v1, v2, v1.length, v2.length))
        println(k)
        (t1._2, t2._2, 1/(k.toDouble+0.1))
      }
      case _ => ???
    }.toList
  }

  def editDistRecur[T](
    s1: Vector[T], s2: Vector[T], m: Int, n: Int
  ): Int = {
    if (m == 0) n*20
    else if (n == 0) m*20
    else if (s1(m-1) == s2(n-1))
      editDist(s1, s2, m-1, n-1)
    else {
      var d = 100000000
      s1(m-1) match {
        case POSTEXPR(y) => {
          s2(n-1) match {
            case POSTEXPR(z) => {
              d = editDist(y.toVector, z.toVector, y.length, z.length)/Math.max(y.length, z.length)
            }
            case _ =>
          }
        }
        case _ =>
      }
      val ret = Math.min(20 + List(
        editDist(s1, s2, m, n-1),
        editDist(s1, s2, m-1, n),
        editDist(s1, s2, m-1, n-1)
      ).reduceLeft(_ min _), d + editDist(s1, s2, m-1, n-1))
      //if(m!=0 && n!=0)
      //println("Now comparing " + s1(m-1) + " " + s2(n-1))
      //println("Reuturning: " + ret)
      ret
    }
  }

  val editDist = Memo.mutableHashMapMemo { (editDistRecur _).tupled }
}
