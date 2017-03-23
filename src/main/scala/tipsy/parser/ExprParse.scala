package tipsy.parser

import tipsy.compiler.{Location, CParserError}
import tipsy.lexer._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

  /**
    * Expression is a general construct which has a return value.
    * It does not have a semi colon at the end.
    */
trait ExprParse extends PackratParsers with Parsers
    with OperatorParsers with Helpers {

  type ExprParse = PackratParser[Expression]

  lazy val identExpr: PackratParser[Expression] =
    identifier ^^ { case a => IdentExpr(a) }

  lazy val literExpr: PackratParser[LiterExpr] =
    literal ^^ { case LITER(a) => LiterExpr(a) }

  lazy val preUnaryExpr: PackratParser[Expression] = preUnaryOp ~ identExpr ^^ {
    case pop ~ e2 => PreUnaryExpr(pop, e2)
  }

  lazy val postUnaryExpr: PackratParser[Expression] =
    identExpr ~ postUnaryOp ^^ {
      case e1 ~ pop => PostUnaryExpr(e1, pop)
    }

  lazy val arrayExpr: PackratParser[Expression] =
    identifier ~ BRACKET(SQUARE(true)) ~ expression ~ BRACKET(SQUARE(false)) ^^ {
      case ident ~ _ ~ index ~ _ => ArrayExpr(ident, index)
    }

  lazy val fxnExpr: PackratParser[Expression] = {
    identifier ~ BRACKET(ROUND(true)) ~
    repsep(expression, comma) ~ BRACKET(ROUND(false)) ^^ {
      case ident ~ _ ~ exprs ~ _ => FxnExpr(ident, exprs)
    }
  }

  lazy val bracketExpr: PackratParser[Expression] = {
    BRACKET(ROUND(true)) ~ expression ~ BRACKET(ROUND(false)) ^^ {
      case _ ~ exp ~ _ => exp
    }
  }

  def prioExprGenerator(prio: Int): ExprParse = positioned {
    lazy val item = {
      if (prio == CLexer.maxLevel) {
        fxnExpr | bracketExpr | arrayExpr |
        preUnaryExpr | postUnaryExpr |
        identExpr | literExpr
      } else {
        chainl1(
          prioExprGenerator(prio+1),
          (prioOp(prio) ^^ {
            case x: BinaryOp => { (a: Expression, b: Expression) =>
              BinaryExpr(a, x, b)
            }
          })
        )
      }
    }
    item
  }

  def condenser(ex: Expression): List[Expression] = {
    ex match {
      case BinaryExpr(e1, BinaryOp(","), e2) => e1 :: condenser(e2)
      case e => List(e)
    }
  }

  lazy val expression: ExprParse = positioned {
    prioExprGenerator(1) ^^ {
      case ex => {
        val condensed = condenser(ex)
        condensed match {
          case x::Nil => x
          case x => CompoundExpr(x)
        }
      }
    }
  }
}
