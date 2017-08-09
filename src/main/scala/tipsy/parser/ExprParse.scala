package tipsy.parser

import tipsy.lexer._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.PackratParsers

  /**
    * Expression is a general construct which has a return value.
    * It does not have a semi colon at the end.
    */
trait ExprParse extends PackratParsers with Parsers
    with OperatorParsers with Helpers {

  type ExprParse = PackratParser[Expression]

  lazy val identExpr: PackratParser[Expression] =
    identifier ^^ { case a => IdentExpr(a) }

  lazy val literExpr: PackratParser[Expression] =
    literal ^^ { case LITER(a) => LiterExpr(a) }

  lazy val preUnaryExpr: PackratParser[Expression] = preUnaryOp ~ (fxnExpr | arrayExpr | identExpr | literExpr | bracketExpr) ^^ {
    case pop ~ e2 => PreUnaryExpr(pop, e2)
  }

  lazy val postUnaryExpr: PackratParser[Expression] =
    (arrayExpr | identExpr | literExpr | bracketExpr) ~ postUnaryOp ^^ {
      case e1 ~ pop => PostUnaryExpr(e1, pop)
    }

  lazy val arrayExpr: PackratParser[Expression] =
    (identifier) ~ rep1(BRACKET(SQUARE(true)) ~ expression ~ BRACKET(SQUARE(false))) ^^ {
      case ident ~ indices => {
        val expressions: List[Expression] = indices.map {
          x => x match {
            case _ ~ expr ~ _ => expr
          }
        }
        ArrayExpr(ident, expressions)
      }
    }

  lazy val fxnExpr: PackratParser[Expression] = {
    identifier ~ BRACKET(ROUND(true)) ~
    expression.? ~ BRACKET(ROUND(false)) ^^ {
      case ident ~ _ ~ None ~ _ => FxnExpr(ident, List(): List[Expression])
      case ident ~ _ ~ Some(CompoundExpr(elist)) ~ _ => FxnExpr(ident, elist)
      case ident ~ _ ~ Some(expr) ~ _ => FxnExpr(ident, List(expr))
    }
  }

  lazy val bracketExpr: PackratParser[Expression] = {
    BRACKET(ROUND(true)) ~ expression ~ BRACKET(ROUND(false)) ^^ {
      case _ ~ exp ~ _ => exp
    }
  }

  lazy val castExpr: PackratParser[Expression] = {
    BRACKET(ROUND(true)) ~ (typename | typenameFromIdent) ~ BRACKET(ROUND(false)) ~
    expression ^^ {
      case _ ~ t ~ _ ~ e => FxnExpr(IDENT(t.toString), List(e))
    }
  }

  lazy val arrayAssignExpr: PackratParser[Expression] = {
    BRACKET(CURLY(true)) ~ repsep(expression, comma) ~ BRACKET(CURLY(false)) ^^ {
      case _ ~ exps ~ _ => CompoundExpr(exps)
    }
  }

  def prioExprGenerator(prio: Int): ExprParse = positioned {
    lazy val item = {
      if (prio == CLexer.maxLevel) {
        castExpr |
        fxnExpr | bracketExpr | arrayExpr |
        preUnaryExpr | postUnaryExpr |
        identExpr | literExpr | arrayAssignExpr
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
      case BinaryExpr(e1, BinaryOp(","), e2) => condenser(e1) ++ condenser(e2)
      case e => List(e)
    }
  }

  lazy val expression: ExprParse = positioned {
    prioExprGenerator(1) ^^ {
      case ex => {
        val condensed = condenser(ex)
        condensed match {
          case x :: Nil => x
          case x => CompoundExpr(x)
        }
      }
    }
  }
}
