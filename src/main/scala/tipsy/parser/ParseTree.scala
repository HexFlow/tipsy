package tipsy.parser

import tipsy.lexer._
import scala.util.parsing.input.Positional

sealed trait CFEnum
case object FUNC extends CFEnum
case class EXPR(value: Expression) extends CFEnum
case class DECL(value: String) extends CFEnum

// Used for storing types
case class QualifiedType(qualifiers: List[String], name: CType) extends ParseTree {
  override def toString(): String = qualifiers.mkString(" ") + " " + name.toString()
}
case class TypedIdent(qt: QualifiedType, name: IDENT) extends ParseTree

// All other ParseTree constructs derive from this
sealed trait ParseTree extends Positional {
  val compress: List[CFEnum] = List()
}

// ParseTree constructs follow =>
// --------------------------- =>

case class TopList(items: List[ParseTree]) extends ParseTree {
  override val compress = items.flatMap(_.compress)
}
case class BlockList(items: List[ParseTree]) extends ParseTree {
  override val compress = items.flatMap(_.compress)
}

// Definitions. Ex: int a = b + 2;
sealed trait Definition extends ParseTree {
  val ti: TypedIdent
}
case class Initialized(ti: TypedIdent, value: Expression) extends Definition {
  override val compress = {
    val k1 = DECL(ti.qt.toString())
    val k2 = EXPR(AssignExpression(ti.name, value))
    List(k1, k2)
  }
}
case class Uninitialized(ti: TypedIdent) extends Definition {
  override val compress = {
    val k1 = DECL(ti.qt.toString())
    List(k1)
  }
}

// A function with type and definition
sealed trait FxnDefinition extends ParseTree {
  val ti: TypedIdent
  val args: List[TypedIdent]
}
case class
  InitializedFxn(ti: TypedIdent, args: List[TypedIdent], body: BlockList)
  extends FxnDefinition {
  override val compress = {
    args.map(_.qt.toString()).sortWith(_<_).map(DECL(_)) ++ body.compress
  }
}
case class
  UninitializedFxn(ti: TypedIdent, args: List[TypedIdent])
    extends FxnDefinition

// A statement. Ex: a += b * 3;
sealed trait Statement extends ParseTree
case class IfStatement(cond: Expression, body: BlockList, elsebody: BlockList)
    extends Statement
case class ForStatement(e1: Expression, e2: Expression,
  e3: Expression, body: BlockList) extends Statement
case class WhileStatement(cond: Expression, body: BlockList) extends Statement
case class DoWhileStatement(body: BlockList, cond: Expression) extends Statement

// Expression constructs follow =>
// ---------------------------- =>

sealed trait Expression extends ParseTree {
  override val compress = {
    List(EXPR(this))
  }
}
case class IdentExpr(id: IDENT) extends Expression
case class LiterExpr(liter: LITER) extends Expression
case class FxnExpr(fxnName: IDENT, exp: Expression) extends Expression
case class PreUnaryExpr(op: UnaryOp, exp: Expression) extends Expression
case class PostUnaryExpr(exp: Expression, op: UnaryOp) extends Expression
case class BinaryExpr(exp1: Expression, op: BinaryOp, exp2: Expression)
    extends Expression
case class AssignExpression(id: IDENT, expr: Expression) extends Expression
