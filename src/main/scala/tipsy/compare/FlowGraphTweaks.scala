package tipsy.compare

import tipsy.parser._

object FlowGraphTweaks {
  var gcnt = 0
  var varsUsed: Map[String, Int] = Map()

  def apply(flow: List[CFEnum]): List[CFEnum] = {
    recur(flow)
  }

  def recur(flow: List[CFEnum]): List[CFEnum] = {
    flow match {
      case Nil => List()
      case x :: xs => {
        val res = x match {
          case DECL(_) => List()
          case x => List(x)
        }

        res ++ recur(xs)
      }
    }
  }
}
