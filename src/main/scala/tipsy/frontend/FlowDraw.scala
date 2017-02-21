package tipsy.frontend

import dot.render._
import dot.contrib._
import dot.core._
import dot.diagram._

import tipsy.parser._

trait FlowDraw {
  implicit def strDrawer: ToRefTree[String] = ToRefTree {
    case x => RefTree.Ref(x, Seq()).rename(x)
  }

  implicit def listDrawer: ToRefTree[List[CFEnum]] = ToRefTree[List[CFEnum]] {
    case x::xs => RefTree.Ref(x, Seq(xs.refTree)).rename(x.flowName)
    case Nil => RefTree.Ref("", Seq()).rename("End")
  }
}
