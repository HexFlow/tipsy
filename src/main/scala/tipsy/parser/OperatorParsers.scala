package tipsy.parser

import tipsy.lexer._

import scala.util.parsing.combinator.Parsers

trait OperatorParsers extends Parsers {
  override type Elem = CToken

  def preUnaryOp: Parser[UnaryOp] = positioned {
    accept("Pre Unary operator", {
      case OPERATOR(pop @ UnaryOp(_)) => pop
      case OPERATOR(ParseBinaryOp(x, _)) if x == "*" || x == "&" =>
        UnaryOp(x)
      case OPERATOR(ParseBinaryOp(x, _)) if x == "-" =>
        UnaryOp(x)
    })
  }

  def postUnaryOp: Parser[UnaryOp] = positioned {
    accept("Post Unary operator", {case OPERATOR(pop @ UnaryOp(_)) => pop})
  }

  def binOp: Parser[BinaryOp] = positioned {
    // Returns an arbitrary binary operator without considering priorities
    accept("Binary op", { case OPERATOR(ParseBinaryOp(x, _)) => BinaryOp(x) })
  }

  def prioOp(prio: Int): Parser[BinaryOp] = positioned {
    accept(s"Operator ${prio}", {
      case OPERATOR(ParseBinaryOp(x, prio2)) if prio2 == prio => BinaryOp(x)
    })
  }

  def rangePrioOp(min: Int, max: Int): Parser[BinaryOp] = positioned {
    accept(s"Operator ${min} to ${max}", {
      case OPERATOR(ParseBinaryOp(x, p)) if p >= min && p <= max => BinaryOp(x)
    })
  }

  def comma: Parser[BinaryOp] = positioned {
    accept("comma", { case OPERATOR(ParseBinaryOp(",", _)) => BinaryOp(",") })
  }

  def operator: Parser[OPERATOR] = positioned {
    accept("operator", {case op @ OPERATOR(_) => op})
  }
}
