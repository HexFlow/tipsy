package tipsy.parser

import tipsy.lexer._
import scala.util.parsing.input.Positional

// Used for storing types
case class QualifiedType(qualifiers: List[String], name: CType) extends ParseTree
case class TypedIdent(qt: QualifiedType, name: IDENT) extends ParseTree

// All other ParseTree constructs derive from this
sealed trait ParseTree extends Positional

// ParseTree constructs follow =>
// --------------------------- =>

case class TopList(items: List[ParseTree]) extends ParseTree
case class BlockList(items: List[ParseTree]) extends ParseTree

// Definitions. Ex: int a = b + 2;
sealed trait Definition extends ParseTree {
  val ti: TypedIdent
}
case class Initialized(ti: TypedIdent, value: ParseTree) extends Definition
case class Uninitialized(ti: TypedIdent) extends Definition

// A function with type and definition
sealed trait FxnDefinition extends ParseTree {
  val ti: TypedIdent
  val args: List[TypedIdent]
}
case class
  InitializedFxn(ti: TypedIdent, args: List[TypedIdent], body: BlockList)
    extends FxnDefinition
case class
  UninitializedFxn(ti: TypedIdent, args: List[TypedIdent])
    extends FxnDefinition

// A statement. Ex: a += b * 3;
case class
  Statement(id: IDENT, op: StatementOp, expr: Expression) extends ParseTree

// Expression constructs follow =>
// ---------------------------- =>

sealed trait Expression extends ParseTree
case class IdentExpr(id: IDENT) extends Expression
case class LiterExpr(liter: LITER) extends Expression
case class FxnExpr(fxnName: IDENT, exp: Expression) extends Expression
case class PreUnaryExpr(op: PreUnaryOp, exp: Expression) extends Expression
case class PostUnaryExpr(exp: Expression, op: PostUnaryOp) extends Expression
case class
  BinaryExpr(exp1: Expression, op: BinaryOp, exp2: Expression) extends Expression
