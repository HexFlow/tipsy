package tipsy.lexer

import scala.util.parsing.input.Positional

sealed trait CToken extends Positional

case class KEYWORD(str: String) extends CToken
case class TYPE(t: CType) extends CToken
case class TYPEQ(qualifier: String) extends CToken
case class BRACKET(t: CBracket) extends CToken
case class IDENT(str: String) extends CToken {
  override def toString(): String = str
}
case class LITER(l: Literal) extends CToken
case class OPERATOR(op: COperator) extends CToken
case class SEMI() extends CToken
case class IF() extends CToken
case class ELSE() extends CToken
case class FOR() extends CToken
case class WHILE() extends CToken
case class DO() extends CToken
case class SWITCH() extends CToken
case class COLON() extends CToken
case class QUESTION() extends CToken

sealed trait Literal extends Positional
case class StrLiteral(s: String) extends Literal {
  override def toString(): String = '"' + s + '"'
}
case class IntLiteral(i: Int) extends Literal {
  override def toString(): String = i.toString
}
case class FloatLiteral(f: Double) extends Literal {
  override def toString(): String = f.toString
}
case class SIZEOF(ct: CType) extends Literal

sealed trait CType extends Positional
case class INT() extends CType {
  override def toString = "int"
}
case class BYTE() extends CType {
  override def toString = "byte"
}
case class CHAR() extends CType {
  override def toString = "char"
}
case class SHORT() extends CType {
  override def toString = "short"
}
case class LONG() extends CType {
  override def toString = "long"
}
case class LONGLONG() extends CType {
  override def toString = "long long"
}
case class FLOAT() extends CType {
  override def toString = "float"
}
case class DOUBLE() extends CType {
  override def toString = "double"
}
case class TYPEPOINTER(typ: CType) extends CType {
  override def toString = typ + "*"
}
case class CUSTOMTYPE(n: String) extends CType {
  override def toString = n
}

sealed trait CBracket extends Positional {
  val open: Boolean
}
case class ROUND(open: Boolean) extends CBracket
case class CURLY(open: Boolean) extends CBracket
case class SQUARE(open: Boolean) extends CBracket

sealed trait COperator extends Positional with CToken {
  val op: String
}
case class StatementOp(op: String) extends COperator
case class UnaryOp(op: String) extends COperator
case class TernaryOp(op: String) extends COperator
case class ParseBinaryOp(op: String, priority: Int) extends COperator
case class BinaryOp(op: String) extends COperator {
  override val toString: String = {
    op match {
      case "<" => "Less than"
      case ">" => "Greater than"
      case "<=" => "Less than equals"
      case ">=" => "Greater than equals"
      case x => x
    }
  }
}
case class CompoundOp(op: String, ops: List[COperator]) extends COperator
