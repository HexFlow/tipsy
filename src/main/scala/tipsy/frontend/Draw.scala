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

  implicit def strDrawer: ToRefTree[String] = ToRefTree {
    case x => RefTree.Ref(x, Seq()).rename(x)
  }

  implicit def typeDrawer: ToRefTree[CType] = ToRefTree[CType] {
    case INT() => IDENT("int").refTree
    case BYTE() => IDENT("byte").refTree
    case CHAR() => IDENT("char").refTree
    case SHORT() => IDENT("short").refTree
    case LONG() => IDENT("long").refTree
    case LONGLONG() => IDENT("long long").refTree
    case FLOAT() => IDENT("float").refTree
    case DOUBLE() => IDENT("double").refTree
    case CUSTOMTYPE(n) => IDENT(n).refTree
  }

  implicit def treeDrawer: ToRefTree[ParseTree] = ToRefTree[ParseTree] {
    case x: QualifiedType => {
      RefTree.Ref(x, x.qualifiers.map(_.refTree) :+ x.name.refTree).rename("Type")
    }

    case x: TypedIdent => {
      RefTree.Ref(x, Seq(x.qt.refTree, x.name.refTree))
        .rename("Typed variable")
    }

    case x @ TopList(items) => {
      RefTree.Ref(x, items.map(_.refTree)).rename("Global")
    }

    case x @ BlockList(items) => {
      RefTree.Ref(x, items.map(_.refTree)).rename("Block")
    }

    case x @ Initialized(ti, value) => {
      RefTree.Ref(x, Seq(ti.refTree, value.refTree))
        .rename("Initialization")
    }

    case x @ Uninitialized(qt) => {
      RefTree.Ref(x, Seq(qt.refTree))
        .rename("Declaration")
    }

    case x @ InitializedFxn(tid, args, body) => {
      RefTree.Ref(x,
        Seq(tid.refTree, args.refTree, body.refTree)).rename("Function")
    }

    case x @ UninitializedFxn(tid, args) => {
      RefTree.Ref(x, Seq(tid.refTree, args.refTree)).rename("Fxn declaration")
    }

    case x: IfStatement => {
      RefTree.Ref(x,
        Seq(x.cond.refTree, x.body.refTree, x.elsebody.refTree))
        .rename("If")
    }

    case x: ForStatement => {
      RefTree.Ref(x, Seq(x.e1.refTree, x.e2.refTree, x.e3.refTree, x.body.refTree))
        .rename("For")
    }

    case x: WhileStatement => {
      RefTree.Ref(x,
        Seq(x.cond.refTree, x.body.refTree)).rename("While")
    }

    case x: DoWhileStatement => {
      RefTree.Ref(x, Seq(x.body.refTree, x.cond.refTree)).rename("Do while")
    }

    case x: Expression => {
      x match {
        case IdentExpr(id) =>
          RefTree.Ref(x, Seq(id.refTree)).rename("IdentExpr")
        case LiterExpr(LITER(li)) =>
          RefTree.Ref(x, Seq()).rename("Literal(" + li + ")")
        case FxnExpr(n, exp) =>
          RefTree.Ref(x, Seq(n.refTree, exp.refTree)).rename("FxnExpr")
        case PreUnaryExpr(op, exp) =>
          RefTree.Ref(x, Seq(op.refTree, exp.refTree)).rename("UnaryExpr")
        case PostUnaryExpr(exp, op) =>
          RefTree.Ref(x, Seq(exp.refTree, op.refTree)).rename("UnaryExpr")
        case BinaryExpr(e, op, f) =>
          RefTree.Ref(x, Seq(e.refTree, op.toString.refTree, f.refTree))
            .rename("BinaryExp")
        case AssignExpression(id, expr) =>
          RefTree.Ref(x, Seq(id.refTree, expr.refTree)).rename("AssignExpr")
      }
    }
  }
}
