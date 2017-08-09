package tipsy.frontend

import reftree.render._
import reftree.contrib._
import reftree.core._
import reftree.diagram._

import tipsy.lexer._
import tipsy.parser._

trait TreeDraw {
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

  implicit def listDrawer = ToRefTree[List[ParseTree]] {
    case x => RefTree.Ref(x, x.map(_.refTree)).rename("List")
  }

  implicit def stringListDrawer = ToRefTree[List[String]] {
    case x => RefTree.Ref(x, x.map(_.refTree)).rename("List")
  }

  implicit def exprDrawer: ToRefTree[Expression] = ToRefTree[Expression] {
    case x @ IdentExpr(id) =>
      RefTree.Ref(x, Seq(id.refTree)).rename("IdentExpr")
    case x @ LiterExpr(li) =>
      RefTree.Ref(x, Seq()).rename("Literal(" + li + ")")
    case x @ FxnExpr(n, exp) =>
      RefTree.Ref(x, Seq(n.refTree, exp.refTree)).rename("FxnExpr")
    case x @ ArrayExpr(n, index) =>
      RefTree.Ref(x, Seq(n.refTree, index.refTree)).rename("ArrayExpr")
    case x @ PreUnaryExpr(op, exp) =>
      RefTree.Ref(x, Seq(op.refTree, exp.refTree)).rename("UnaryExpr")
    case x @ PostUnaryExpr(exp, op) =>
      RefTree.Ref(x, Seq(exp.refTree, op.refTree)).rename("UnaryExpr")
    case x @ BinaryExpr(e, op, f) =>
      RefTree.Ref(x, Seq(e.refTree, op.toString.refTree, f.refTree))
        .rename("BinaryExp")
    case x @ CompoundExpr(exprs) =>
      RefTree.Ref(x, exprs.map(_.refTree)).rename("CompoundExpr")
  }

  implicit def treeDrawer: ToRefTree[ParseTree] = ToRefTree[ParseTree] {
    case x: QualifiedType =>
      RefTree.Ref(x, x.qualifiers.map(_.refTree) :+ x.name.refTree).rename("Type")

    case x @ TopList(items) =>
      RefTree.Ref(x, items.map(_.refTree)).rename("Global")

    case x @ BlockList(items) =>
      RefTree.Ref(x, items.map(_.refTree)).rename("Block")

    case x @ Definition(ty, id, value) => {
      val name = value match {
        case None => "Declaration"
        case _ => "Definition"
      }
      RefTree.Ref(
        x, Seq(ty.refTree) ++ Seq(id.refTree) ++
          value.map(y => List(y.refTree)).getOrElse(Seq())
      ).rename(name)
    }

    case x @ Definitions(defs) => {
      RefTree.Ref(x, defs.map(y => y.refTree)).rename("Definitions")
    }

    case x @ FxnDefinition(tid, args, body) => {
      val name = body match {
        case None => "Function declaration"
        case _ => "Function definition"
      }
      RefTree.Ref(x, Seq(tid.refTree, args.refTree) ++ body.toList.map(_.refTree))
        .rename(name)
    }

    case x: Break =>
      RefTree.Ref(x, Seq())
        .rename("Break")

    case x: Continue =>
      RefTree.Ref(x, Seq())
        .rename("Continue")
    case x: IfStatement =>
      RefTree.Ref(x, Seq(x.cond.refTree, x.body.refTree, x.elsebody.refTree))
        .rename("If")

    case x: ForStatement =>
      RefTree.Ref(x, Seq(x.e1.refTree, x.e2.refTree, x.e3.refTree, x.body.refTree))
        .rename("For")

    case x: WhileStatement =>
      RefTree.Ref(x, Seq(x.cond.refTree, x.body.refTree)).rename("While")

    case x: DoWhileStatement =>
      RefTree.Ref(x, Seq(x.body.refTree, x.cond.refTree)).rename("Do while")

    case x: ReturnStatement =>
      RefTree.Ref(x, Seq(x.code.refTree)).rename("Return")

    case x: Expression => x.refTree
  }
}
