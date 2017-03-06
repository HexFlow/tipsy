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
case class COMMA() extends CToken

case class IF() extends CToken
case class ELSE() extends CToken
case class FOR() extends CToken
case class WHILE() extends CToken
case class DO() extends CToken

sealed trait Literal extends Positional
case class StrLiteral(s: String) extends Literal
case class IntLiteral(i: Int) extends Literal
case class FloatLiteral(f: Double) extends Literal

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
case class CUSTOMTYPE(n: String) extends CType {
  override def toString = n
}

sealed trait CBracket { val open: Boolean }
case class ROUND(open: Boolean) extends CBracket
case class CURLY(open: Boolean) extends CBracket
case class SQUARE(open: Boolean) extends CBracket

sealed trait COperator extends Positional with CToken {
  val op: String
}
case class StatementOp(op: String) extends COperator
case class UnaryOp(op: String) extends COperator
case class TernaryOp(op: String) extends COperator
case class ParseBinaryOp(prio: PriorityBinaryOperator) extends COperator {
  val op = prio.op
}
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

sealed trait PriorityBinaryOperator extends COperator with CToken {
  val op: String
}
case class Prio1(op: String) extends PriorityBinaryOperator
case class Prio2(op: String) extends PriorityBinaryOperator
case class Prio3(op: String) extends PriorityBinaryOperator
case class Prio4(op: String) extends PriorityBinaryOperator
