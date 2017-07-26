package tipsy.parser

import tipsy.lexer._
import scala.util.parsing.input.Positional

sealed trait CFEnum {
  val flowName: String
}
case object FUNC extends CFEnum {
  val flowName = "Func"
}
case class RETTYPE(ret: String) extends CFEnum {
  val flowName = "Returns " + ret
}
case class EXPR(value: Expression) extends CFEnum {
  val flowName = "Expression"
}
case class DECL(value: String) extends CFEnum {
  val flowName = "Declaration: " + value
}
case object IFCOND extends CFEnum {
  val flowName = "If"
}
case object SWITCHCOND extends CFEnum {
  val flowName = "Switch"
}
case object LOOPCOND extends CFEnum {
  val flowName = "Loop"
}
case class POSTEXPR(value: List[String]) extends CFEnum {
  val flowName = "PostscriptExpr"
}
case object RETURN extends CFEnum {
  val flowName = "Return"
}
case object BLOCKOPEN extends CFEnum {
  val flowName = "Block Open"
}
case object BLOCKCLOSE extends CFEnum {
  val flowName = "Block Close"
}
class FLOWMANIPULATION() extends CFEnum {
  val flowName = "Flow Manipulation"
}
case object CONT extends FLOWMANIPULATION {
  override val flowName = "Continue"
}
case object BRK extends FLOWMANIPULATION {
  override val flowName = "Break"
}

// Used for storing types
case class QualifiedType(qualifiers: List[String], name: CType) extends ParseTree {
  override def toString(): String = (qualifiers :+ name.toString).mkString(" ")
}
case class TypedIdent(qt: QualifiedType, name: Option[IDENT]) extends ParseTree

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

case class Definitions(defs: List[Definition]) extends ParseTree {
  // Does not exist in the final AST, courtesy customFlatten
  override val compress = {
    defs.flatMap(_.compress)
  }
}

// Definitions. Ex: int a = b + 2;
case class Definition(ty: QualifiedType, id: Option[Expression],
  value: Option[Expression]) extends ParseTree {

  override val compress = {
    // Note: Removed declarations from flow graph
    // DECL(ty.toString()) ::
    value.map { expr =>
      BinaryExpr(id.get, BinaryOp("="), expr).compress
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
    FUNC ::
    RETTYPE(ti.qt.toString) ::
    args.map(_.qt.toString()).sortWith(_<_).map(x => DECL("Argument " + x)) ++
    body.map(_.compress).getOrElse(List())
  }
}

sealed trait FlowStatement extends ParseTree
case class Break() extends FlowStatement {
  override val compress = List(BRK)
}
case class Continue() extends FlowStatement {
  override val compress = List(CONT)
}

// A statement. Ex: a += b * 3;
sealed trait Statement extends ParseTree
case class IfStatement(cond: Expression, body: BlockList,
  elsebody: BlockList) extends Statement {
  override val compress = {
    IFCOND :: EXPR(cond) :: body.compress ++ elsebody.compress
  }
}

case class SwitchStatement(value: Expression, caseBlocks: List[(Expression, BlockList)], defaultBlock: BlockList) extends Statement {
  override val compress = {
    SWITCHCOND :: EXPR(value) :: caseBlocks.flatMap(x => EXPR(x._1) :: x._2.compress) ++ defaultBlock.compress
  }
}

case class ForStatement(e1: Expression, e2: Expression,
  e3: Expression, body: BlockList) extends Statement {
  override val compress =
    EXPR(e1) :: LOOPCOND :: EXPR(e2) ::
  body.copy(items = body.items :+ e3).compress
}

case class WhileStatement(cond: Expression,
  body: BlockList) extends Statement {
  override val compress = LOOPCOND :: EXPR(cond) :: body.compress
}

case class DoWhileStatement(body: BlockList,
  cond: Expression) extends Statement {
  lazy val compressedBody: List[CFEnum] = body.compress
  override val compress = compressedBody ++ List(LOOPCOND, EXPR(cond)) ++ compressedBody
}

case class ReturnStatement(code: Expression) extends Statement {
  override val compress = RETURN :: code.compress
}

// Expression constructs follow =>
// ---------------------------- =>

sealed trait Expression extends ParseTree {
  override val compress = List(EXPR(this))

  // Provide list of functions used in this expression in order of use
  val getFxns: List[String] = {
    this match {
      case ArrayExpr(_, indices) => indices.flatMap(_.getFxns)
      case FxnExpr(name, _) => List(name.str)
      case PreUnaryExpr(_, e) => e.getFxns
      case PostUnaryExpr(e, _) => e.getFxns
      case BinaryExpr(e1, _, e2) => e1.getFxns ++ e2.getFxns
      case CompoundExpr(e) => e.flatMap(_.getFxns)
      case _ => List()
    }
  }
}

case class IdentExpr(id: IDENT) extends Expression {
  override def toString() = id.toString
}
case class LiterExpr(liter: Literal) extends Expression {
  override def toString() = liter.toString
}
case class ArrayExpr(name: IDENT, index: List[Expression]) extends Expression
case class FxnExpr(fxnName: IDENT, exp: List[Expression]) extends Expression
case class PreUnaryExpr(op: UnaryOp, exp: Expression) extends Expression
case class PostUnaryExpr(exp: Expression, op: UnaryOp) extends Expression
case class BinaryExpr(exp1: Expression, op: BinaryOp, exp2: Expression)
    extends Expression
case class CompoundExpr(exprs: List[Expression]) extends Expression
