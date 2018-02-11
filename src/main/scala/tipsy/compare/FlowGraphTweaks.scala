package tipsy.compare

import tipsy.parser._
import tipsy.lexer._

object FlowGraphTweaks {

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

  def post(e: Expression): CFEnum = {
    val p = POSTEXPR(renameIdentsInExpr(e))
    return p.setPos(e.pos)
  }

  def renameIdentsInExpr(e: Expression): List[String] = {
    var gcnt = 0
    var varsUsed: Map[String, Int] = Map()

    // Does the actual renaming / query-map logic
    def renameIdent(name: String): String = {
      if (varsUsed isDefinedAt name) {
        "var" + varsUsed(name).toString
      } else {
        gcnt = gcnt + 1
        varsUsed += (name -> gcnt)
        "var" + gcnt.toString
      }
    }

    // Main recursive routine to rename expressions
    def renameRecur(e: Expression): List[String] = {
      e match {
        case IdentExpr(IDENT(i)) => List(renameIdent(i))
        case ArrayExpr(IDENT(i), index) =>
          List(renameIdent(i))
        case FxnExpr(IDENT(i), exps) =>
          exps.flatMap(renameRecur(_)) ++ List(s"func:${i.toString}")
        case PreUnaryExpr(op, exp) => renameRecur(exp) ++ List(op.toString)
        case PostUnaryExpr(exp, op) => renameRecur(exp) ++ List(op.toString)
        case BinaryExpr(e1, op, e2) =>
          renameRecur(e1) ++ renameRecur(e2) ++ List(op.toString)
        case CompoundExpr(exprs) => exprs.flatMap(renameRecur(_))
        case LiterExpr(x) => List(x.toString)
        //case x => x
      }
    }
    renameRecur(e)
  }

  def renameFxnNames(lenums: List[CFEnum]): List[CFEnum] = {
    var gcnt = 0
    var varsUsed: Map[String, Int] = Map()

    // Does the actual renaming / query-map logic
    def renameIdent(name: String): String = {
      if (varsUsed isDefinedAt name) {
        "func" + varsUsed(name).toString
      } else {
        gcnt = gcnt + 1
        varsUsed += (name -> gcnt)
        "func" + gcnt.toString
      }
    }

    lenums.map {
      case orig@(POSTEXPR(e)) => POSTEXPR(e.map {
        case x if x.startsWith("func:") => renameIdent(x)
        case y => y
      }).setPos(orig)
      case x => x
    }
  }

}
