package tipsy.parser

import tipsy.lexer._
import scala.util.parsing.input.Positional

sealed trait CFEnum {
  val flowName: String
}
case class ASSIGN(expr: Expression) extends CFEnum {
  val flowName = "Assignment"
}
case class FUNC(returnType: String) extends CFEnum {
  val flowName = "Func: " + returnType
}
case class EXPR(value: Expression) extends CFEnum {
  val flowName = "Expression"
}
case class DECL(value: String) extends CFEnum {
  val flowName = "Declaration: " + value
}
case class IFCOND(value: Expression) extends CFEnum {
  val flowName = "If"
}
case class LOOPCOND(value: Expression) extends CFEnum {
  val flowName = "Loop"
}

case object BLOCKOPEN extends CFEnum {
  val flowName = "Block Open"
}

case object BLOCKCLOSE extends CFEnum {
  val flowName = "Block Close"
}

// Used for storing types
case class QualifiedType(qualifiers: List[String], name: CType) extends ParseTree {
  override def toString(): String = (qualifiers :+ name.toString).mkString(" ")
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
  override val compress =
    BLOCKOPEN :: items.flatMap(_.compress) ++ List(BLOCKCLOSE)
}

// Definitions. Ex: int a = b + 2;
case class Definition(ti: TypedIdent, value: Option[Expression])
    extends ParseTree {
  override val compress = {
    // Note: Removed declarations from flow graph
    // DECL(ti.qt.toString()) ::
    value.map { expr =>
      AssignExpression(ti.name, expr).compress
    }.getOrElse(List())
  }
}

// A function with type and definition
case class FxnDefinition(
  ti: TypedIdent,
  args: List[TypedIdent],
  body: Option[BlockList]
) extends ParseTree {
  // If body is None, it won't show BlockOpen and BlockClose
  override val compress = {
    FUNC(ti.qt.toString) ::
    args.map(_.qt.toString()).sortWith(_<_).map(x => DECL("Argument " + x)) ++
    body.map(_.compress).getOrElse(List())
  }
}

// A statement. Ex: a += b * 3;
sealed trait Statement extends ParseTree
case class IfStatement(cond: Expression, body: BlockList,
  elsebody: BlockList) extends Statement {
  override val compress = {
    IFCOND(cond) :: body.compress ++ elsebody.compress
  }
}

case class ForStatement(e1: Expression, e2: Expression,
  e3: Expression, body: BlockList) extends Statement {
  EXPR(e1) :: LOOPCOND(e2) :: List(body.copy(items = body.items :+ e3))
}

case class WhileStatement(cond: Expression,
  body: BlockList) extends Statement {
    LOOPCOND(cond) :: body.compress
}

case class DoWhileStatement(body: BlockList,
  cond: Expression) extends Statement {
    LOOPCOND(cond) :: body.compress
}

// Expression constructs follow =>
// ---------------------------- =>

sealed trait Expression extends ParseTree {
  override val compress = {
    this match {
      case AssignExpression(_, _) => List(ASSIGN(this))
      case _ => List(EXPR(this))
    }
  }
}

case class IdentExpr(id: IDENT) extends Expression
case class LiterExpr(liter: LITER) extends Expression
case class ArrayExpr(name: IDENT, index: Expression) extends Expression
case class FxnExpr(fxnName: IDENT, exp: List[Expression]) extends Expression
case class PreUnaryExpr(op: UnaryOp, exp: Expression) extends Expression
case class PostUnaryExpr(exp: Expression, op: UnaryOp) extends Expression
case class BinaryExpr(exp1: Expression, op: BinaryOp, exp2: Expression)
    extends Expression
case class AssignExpression(id: IDENT, expr: Expression) extends Expression
