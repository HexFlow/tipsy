package tipsy.frontend

import reftree.render._
import reftree.contrib._
import reftree.core._
import reftree.diagram._
import java.nio.file.Paths

import tipsy.lexer._
import tipsy.parser._

trait Draw {
  implicit def idDrawer: ToRefTree[IDENT] = ToRefTree {
    case x @ IDENT(v) => RefTree.Ref(x, Seq()).rename("Identifier(" + v + ")")
  }

  implicit def literDrawer: ToRefTree[LITER] = ToRefTree {
    case x @ LITER(v) => RefTree.Ref(x, Seq()).rename("Literal(" + v + ")")
  }

  implicit def copDrawer: ToRefTree[COperator] = ToRefTree {
    case x: COperator => RefTree.Ref(x, Seq()).rename("Operator(" + x.op + ")")
  }

  implicit def typeDrawer: ToRefTree[CType] = ToRefTree[CType] {
    case INT() => IDENT("int").refTree
    case _ => "some type".refTree
  }

  implicit def treeDrawer: ToRefTree[ParseTree] = ToRefTree[ParseTree] {

    case x: QualifiedType => {
      RefTree.Ref(x, x.qualifiers.map(_.refTree) :+ x.name.refTree).rename("Type")
    }

    case x @ DefinitionList(d1, d2) => {
      RefTree.Ref(x, Seq(d1.refTree, d2.refTree))
    }

    case x @ Definition(qt, name, value) => {
      RefTree.Ref(x, Seq(qt.refTree, name.refTree, value.refTree))
        .rename("Definition")
    }

    case x @ UnDefinition(qt, name) => {
      RefTree.Ref(x, Seq(qt.refTree, name.refTree))
        .rename("Definition")
    }

    case x: Statement => {
      RefTree.Ref(x, Seq(x.id.refTree, x.op.refTree, x.expr.refTree)
      ).rename("Statement")
    }

    case x: Expression => {
      x match {
        case IdentExpr(id) => RefTree.Ref(x, Seq(id.refTree)).rename("IdentExpr")
        case LiterExpr(li) => RefTree.Ref(x, Seq(li.refTree)).rename("LiterExpr")
        case FxnExpr(n, exp) =>
          RefTree.Ref(x, Seq(n.refTree, exp.refTree)).rename("FxnExpr")
        case PreUnaryExpr(op, exp) =>
          RefTree.Ref(x, Seq(op.refTree, exp.refTree)).rename("UnaryExpr")
        case PostUnaryExpr(exp, op) =>
          RefTree.Ref(x, Seq(exp.refTree, op.refTree)).rename("UnaryExpr")
        case BinaryExpr(e, op, f) =>
          RefTree.Ref(x, Seq(e.refTree, op.refTree, f.refTree)).rename("BinaryExp")
      }
    }

    case x @ FunctionDefinition(typ, id, defs) => {
      RefTree.Ref(x,
        typ.refTree :: id.refTree :: defs.map(_.refTree)).rename("Function")
    }

    case _ => RefTree.Null()
  }
}
