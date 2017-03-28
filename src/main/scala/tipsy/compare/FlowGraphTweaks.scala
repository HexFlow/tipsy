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

  def renameIdentsInExpr(e: Expression): Expression = {
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
    def renameRecur(e: Expression): Expression = {
      e match {
        case IdentExpr(IDENT(i)) => IdentExpr(IDENT(renameIdent(i)))
        case ArrayExpr(IDENT(i), index) =>
          ArrayExpr(IDENT(renameIdent(i)), renameIdentsInExpr(index))
        case FxnExpr(IDENT(i), exps) =>
          FxnExpr(IDENT(renameIdent(i)), exps.map(renameRecur(_)))
        case PreUnaryExpr(op, exp) => PreUnaryExpr(op, renameRecur(exp))
        case PostUnaryExpr(exp, op) => PostUnaryExpr(renameRecur(exp), op)
        case BinaryExpr(e1, op, e2) =>
          BinaryExpr(renameRecur(e1), op, renameRecur(e2))
        case CompoundExpr(exprs) => CompoundExpr(exprs.map(renameRecur(_)))
        case x => x
      }
    }

    renameRecur(e)
  }
}
