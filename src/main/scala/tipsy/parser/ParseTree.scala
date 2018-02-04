package tipsy.parser

import tipsy.lexer._
import tipsy.compare.FlowGraphTweaks.{ renameIdentsInExpr => r }
import scala.util.parsing.input.{Positional, Position}

@SerialVersionUID(100L)
sealed trait CFEnum extends Serializable {
  val flowName: String

  var line: Int = 0
  var column: Int = 0
  def setPos(p: Position): CFEnum = {
    if (line == 0) line = p.line
    if (column == 0) column = p.column
    return this
  }
}
case class FUNC() extends CFEnum {
  val flowName = "Func"
}
case class RETTYPE(ret: String) extends CFEnum {
  val flowName = "Returns " + ret
}
case class DECL(value: String) extends CFEnum {
  val flowName = "Declaration: " + value
}
case class IFCOND() extends CFEnum {
  val flowName = "If"
}
case class SWITCHCOND() extends CFEnum {
  val flowName = "Switch"
}
case class LOOPCOND() extends CFEnum {
  val flowName = "Loop"
}
case class POSTEXPR(value: List[String]) extends CFEnum {
  val flowName = "PostscriptExpr"
}
case class RETURN() extends CFEnum {
  val flowName = "Return"
}
case class BLOCKOPEN() extends CFEnum {
  val flowName = "Block Open"
}
case class BLOCKCLOSE() extends CFEnum {
  val flowName = "Block Close"
}
case class CONT() extends CFEnum {
  val flowName = "Continue"
}
case class BRK() extends CFEnum {
  val flowName = "Break"
}

// Used for storing types
case class QualifiedType(qualifiers: List[String], name: CType) extends ParseTree {
  override def toString(): String = (qualifiers :+ name.toString).mkString(" ")
}
case class TypedIdent(qt: QualifiedType, name: IDENT) extends ParseTree

/**
  * All other ParseTree constructs derive from this superclass.
  * This is a positioned type, and contains the structure of the parsed
  * program, along with corresponding locations marked in the object as _.pos.
  *
  * It implements the following methods:
  * _.compress: This returns a list of CFEnums with positions set correctly.
  * _.rawCompress: This is used by compress, and the list returned by this does
  *                not contain the positions.
  */
sealed trait ParseTree extends Positional {
  lazy val compress: List[CFEnum] = rawCompress.map(_.setPos(pos))
  lazy val rawCompress: List[CFEnum] = List()
}

// ParseTree constructs follow =>
// --------------------------- =>

case class TopList(items: List[ParseTree]) extends ParseTree {
  override lazy val rawCompress = items.flatMap(_.compress)
}
case class BlockList(items: List[ParseTree]) extends ParseTree {
  override lazy val rawCompress =
    (BLOCKOPEN() :: items.flatMap(_.compress) ++ List(BLOCKCLOSE()))
}

case class Definitions(defs: List[Definition]) extends ParseTree {
  // Does not exist in the final AST, courtesy customFlatten
  override lazy val rawCompress = {
    defs.flatMap(_.compress)
  }
}

// Definitions. Ex: int a = b + 2;
case class Definition(ty: QualifiedType, id: Option[Expression],
  value: Option[Expression]) extends ParseTree {

  override lazy val rawCompress = {
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
  override lazy val rawCompress = {
    FUNC().setPos(pos) ::
    RETTYPE(ti.qt.toString) ::
    args.map(_.qt.toString()).sortWith(_<_).map(x => DECL("Argument " + x)) ++
    body.map(_.compress).getOrElse(List())
  }
}

sealed trait FlowStatement extends ParseTree
case class Break() extends FlowStatement {
  override lazy val rawCompress = List(BRK())
}
case class Continue() extends FlowStatement {
  override lazy val rawCompress = List(CONT())
}

// A statement. Ex: a += b * 3;
sealed trait Statement extends ParseTree
case class IfStatement(cond: Expression, body: BlockList,
  elsebody: BlockList) extends Statement {
  override lazy val rawCompress = {
    IFCOND() :: POSTEXPR(r(cond)) :: body.compress ++ elsebody.compress
  }
}

case class SwitchStatement(value: Expression, caseBlocks: List[(Expression, BlockList)],
  defaultBlock: BlockList) extends Statement {
  override lazy val rawCompress = {
    SWITCHCOND() :: POSTEXPR(r(value)) :: caseBlocks.flatMap(x => POSTEXPR(r(x._1)) ::
      x._2.compress) ++ defaultBlock.compress
  }
}

case class ForStatement(e1: Expression, e2: Expression,
  e3: Expression, body: BlockList) extends Statement {
  override lazy val rawCompress =
    POSTEXPR(r(e1)) :: LOOPCOND() :: POSTEXPR(r(e2)) ::
  body.copy(items = body.items :+ e3).compress
}

case class WhileStatement(cond: Expression,
  body: BlockList) extends Statement {
  override lazy val rawCompress = LOOPCOND() :: POSTEXPR(r(cond)) :: body.compress
}

case class DoWhileStatement(body: BlockList,
  cond: Expression) extends Statement {
  lazy val compressedBody: List[CFEnum] = body.compress
  override lazy val rawCompress = compressedBody ++ List(LOOPCOND(), POSTEXPR(r(cond))) ++ compressedBody
}

case class ReturnStatement(code: Expression) extends Statement {
  override lazy val rawCompress = RETURN() :: code.compress
}

// Expression constructs follow =>
// ---------------------------- =>

sealed trait Expression extends ParseTree {
  override lazy val rawCompress = List(POSTEXPR(r(this)))

  // Provide list of functions used in this expression in order of use
  val getFxns: List[String] = {
    this match {
      case ArrayExpr(_, indices) => indices.flatMap(_.getFxns)
      case FxnExpr(name, e) => name.str :: e.flatMap(_.getFxns)
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
