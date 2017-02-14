package tipsy.parser

import tipsy.compiler.{Location, CParserError}
import tipsy.lexer._

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

trait OperatorParsers extends Parsers {
  override type Elem = CToken

  def postOp: Parser[PostUnaryOp] = positioned {
    accept("Post unary operator", { case OPERATOR(pop @ PostUnaryOp(_)) => pop })
  }

  def preOp: Parser[PreUnaryOp] = positioned {
    accept("Pre unary operator", { case OPERATOR(pop @ PreUnaryOp(_)) => pop })
  }

  def binOp: Parser[BinaryOp] = positioned {
    // Returns an arbitrary binary operator without considering priorities
    accept("Binary op", { case OPERATOR(ParseBinaryOp(x)) => BinaryOp(x.op) })
  }

  def prio1op: Parser[BinaryOp] = positioned {
    accept("prio1op", { case OPERATOR(ParseBinaryOp(Prio1(x))) => BinaryOp(x) })
  }

  def prio2op: Parser[BinaryOp] = positioned {
    accept("prio2op", { case OPERATOR(ParseBinaryOp(Prio2(x))) => BinaryOp(x) })
  }

  def prio3op: Parser[BinaryOp] = positioned {
    accept("prio3op", { case OPERATOR(ParseBinaryOp(Prio3(x))) => BinaryOp(x) })
  }

  def prio4op: Parser[BinaryOp] = positioned {
    accept("prio4op", { case OPERATOR(ParseBinaryOp(Prio4(x))) => BinaryOp(x) })
  }

  def operator: Parser[OPERATOR] = positioned {
    accept("operator", {case op @ OPERATOR(_) => op})
  }
}
