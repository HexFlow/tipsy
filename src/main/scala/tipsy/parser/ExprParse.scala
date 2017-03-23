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
    repsep(expression, COMMA()) ~ BRACKET(ROUND(false)) ^^ {
      case ident ~ _ ~ exprs ~ _ => FxnExpr(ident, exprs)
    }
  }

  lazy val bracketExpr: PackratParser[Expression] = {
    BRACKET(ROUND(true)) ~ expression ~ BRACKET(ROUND(false)) ^^ {
      case _ ~ exp ~ _ => exp
    }
  }

  type ExprOp = (Expression, BinaryOp)

  /**
    * Takes a binary operator parser.
    * Returns a parser which parses an expression followed
    * by that binary operator
    */
  def getPairParser(opP: => Parser[BinaryOp],
    ex: ExprParse = expression): Parser[ExprOp] = {
    ex ~ opP ^^ {
      case expr ~ op => (expr, op)
    }
  }

  /**
    * Takes a list of (expression, binary operator), and a
    * final expression (of lower priority) after the list.
    *
    * Returns a left recursive parse tree of the above
    */
  def getTreeFromExprList(lis: List[ExprOp], id: Expression) = {
    val parseList: List[ExprOp] = lis :+ (id, BinaryOp("*"))

    (parseList match {
      case x :: xs => {
        xs.foldLeft (x) {
          (prevRes: ExprOp, nEO: ExprOp) => {
            (BinaryExpr(prevRes._1, prevRes._2, nEO._1), nEO._2)
          }
        }
      }
      case _ => ???
    })._1
  }

  lazy val prio1Expr: ExprParse = positioned {
    rep(getPairParser(prio1op | prio2op | prio3op | prio4op)) ~
    (prio2Expr | prio3Expr | prio4Expr | prio5Expr) ^^ {
      case lis ~ id => getTreeFromExprList(lis, id)
    }
  }

  lazy val prio2Expr: ExprParse = positioned {
    rep(getPairParser(
      prio2op | prio3op | prio4op)) ~ (prio3Expr | prio4Expr | prio5Expr) ^^ {
      case lis ~ id => getTreeFromExprList(lis, id)
    }
  }

  lazy val prio3Expr: ExprParse = positioned {
    rep(getPairParser(prio3op | prio4op)) ~ (prio4Expr | prio5Expr) ^^ {
      case lis ~ id => getTreeFromExprList(lis, id)
    }
  }

  lazy val prio4Expr: ExprParse = positioned {
    rep(getPairParser(prio4op)) ~ prio5Expr ^^ {
      case lis ~ id => getTreeFromExprList(lis, id)
    }
  }

  lazy val prio5Expr: ExprParse = positioned {
    fxnExpr | bracketExpr | arrayExpr |
    preUnaryExpr | postUnaryExpr |
    identExpr | literExpr
  }

  lazy val assignExpr: PackratParser[AssignExpr] = {
    (preUnaryExpr | postUnaryExpr | arrayExpr | identExpr) ~
    OPERATOR(BinaryOp("=")) ~ simpleExpr ^^ {
      case eid ~ op ~ expr => AssignExpr(eid, expr)
    }
  }

  lazy val simpleExpr: ExprParse = positioned { assignExpr | prio1Expr }

  lazy val compoundExpr: ExprParse = {
    repsep(simpleExpr, COMMA()) ^^ {
      case expr :: Nil => expr
      case exprs => CompoundExpr(exprs)
    }
  }

  lazy val expression: ExprParse = positioned { compoundExpr }
}
