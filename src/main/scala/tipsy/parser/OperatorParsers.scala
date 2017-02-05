package tipsy.parser

import tipsy.compiler.{Location, CParserError}
import tipsy.lexer._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

trait OperatorParsers extends Parsers {
  override type Elem = CToken

  def postOp: Parser[PostUnaryOp] = positioned {
    operator ^^ { case OPERATOR(pop @ PostUnaryOp(_)) => pop }
  }

  def preOp: Parser[PreUnaryOp] = positioned {
    operator ^^ { case OPERATOR(pop @ PreUnaryOp(_)) => pop }
  }

  def binOp: Parser[BinaryOp] = positioned {
    operator ^^ { case OPERATOR(bop @ BinaryOp(_)) => bop }
  }

  def stmtOp: Parser[StatementOp] = positioned {
    operator ^^ { case OPERATOR(sop @ StatementOp(_)) => sop }
  }

  def prio1op: Parser[BinaryOp] = positioned {
    accept("prio1op", { case OPERATOR(bop @ BinaryOp(Prio1(_))) => bop })
  }

  def prio2op: Parser[BinaryOp] = positioned {
    accept("prio2op", { case OPERATOR(bop @ BinaryOp(Prio2(_))) => bop })
  }

  def prio3op: Parser[BinaryOp] = positioned {
    accept("prio3op", { case OPERATOR(bop @ BinaryOp(Prio3(_))) => bop })
  }

  def prio4op: Parser[BinaryOp] = positioned {
    accept("prio4op", { case OPERATOR(bop @ BinaryOp(Prio4(_))) => bop })
  }

  def operator: Parser[OPERATOR] = positioned {
    accept("operator", {case op @ OPERATOR(_) => op})
  }
}
