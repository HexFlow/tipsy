package tipsy.lexer

import scala.util.parsing.input.Positional

sealed trait CToken extends Positional

case class KEYWORD(str: String) extends CToken
case class TYPE(t: CType) extends CToken
case class TYPEQ(qualifier: String) extends CToken
case class BRACKET(t: CBracket) extends CToken
case class IDENT(str: String) extends CToken
case class LITER(l: Literal) extends CToken
case class OPERATOR(op: COperator) extends CToken
case class SEMI() extends CToken

sealed trait Literal extends Positional
case class StrLiteral(s: String) extends Literal
case class IntLiteral(i: Int) extends Literal
case class FloatLiteral(f: Double) extends Literal

sealed trait CType extends Positional
case class INT() extends CType
case class BYTE() extends CType
case class SHORT() extends CType
case class LONG() extends CType
case class LONGLONG() extends CType
case class FLOAT() extends CType
case class DOUBLE() extends CType
case class STRING() extends CType

sealed trait CBracket { val open: Boolean }
case class ROUND(open: Boolean) extends CBracket
case class CURLY(open: Boolean) extends CBracket
case class SQUARE(open: Boolean) extends CBracket

sealed trait COperator extends Positional
case class StatementOp(op: String) extends COperator
case class PreUnaryOp(op: String) extends COperator
case class PostUnaryOp(op: String) extends COperator
case class BinaryOp(op: String) extends COperator
case class TernaryOp(op: String) extends COperator
