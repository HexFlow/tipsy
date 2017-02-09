package tipsy.parser

import tipsy.compiler.{Location, CParserError}
import tipsy.lexer._

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object CPackParser extends PackratParsers with Parsers with OperatorParsers {
  override type Elem = CToken

  type TreeParse = PackratParser[ParseTree]
  type ExprParse = PackratParser[Expression]

  class CTokenReader(tokens: Seq[Elem]) extends Reader[Elem] {
    override def first: Elem = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest: Reader[Elem] = new CTokenReader(tokens.tail)
  }


  def apply(tokens: Seq[Elem]): Either[CParserError, ParseTree] = {
    val reader = new PackratReader(new CTokenReader(tokens))
    program(reader) match {
      case NoSuccess(msg, next) =>
        Left(CParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  lazy val program: TreeParse = positioned {
    // Program comprises of statementList
    phrase(statementList)
  }

  lazy val statementList: TreeParse = positioned {
    // It has a lot of statements
    rep1(globalDefinitions) ^^ { case x => x reduceRight DefinitionList }
  }

  lazy val globalDefinitions: TreeParse = positioned {
    // Statement may be a function or a regular definition
    functionDefinition | definition
  }

  lazy val definition: TreeParse = positioned {
    val uninitialized = {
      typeparse ~ identifier ~ SEMI() ^^ {
        case qualifiedType ~ IDENT(id) ~ _ => {
          UnDefinition(qualifiedType, id)
        }
      }
    }

    val initialized = {
      typeparse ~ identifier ~
      OPERATOR(StatementOp("=")) ~
      expression ~ SEMI() ^^ {

        case qualifiedType ~ IDENT(id) ~ _ ~ expr ~ _ => {
          Definition(qualifiedType, id, expr)
        }
      }
    }

    // A regular definition will either be uninitialized, or initialized
    uninitialized | initialized
  }

  lazy val functionDefinition: TreeParse = positioned {
    typeparse ~ identifier ~
    BRACKET(ROUND(true)) ~
    BRACKET(ROUND(false)) ~
    BRACKET(CURLY(true)) ~
    rep(definition | statement) ~
    BRACKET(CURLY(false)) ^^ {

      case qualifiedType ~ IDENT(id) ~ _ ~ _ ~ _ ~ defs ~ _ =>
        FunctionDefinition(qualifiedType, id, defs)
    }
  }

  lazy val statement: TreeParse = positioned {
    // Returns a complete statement
    identifier ~ stmtOp ~ expression ~ SEMI() ^^ {
      case id ~ op ~ expr ~ _ => Statement(id, op, expr)
    }
  }

  lazy val expression: ExprParse = positioned {

    lazy val identExpr: PackratParser[IdentExpr] =
      identifier ^^ { case a @ IDENT(_) => IdentExpr(a) }

    lazy val literExpr: PackratParser[LiterExpr] =
      literal ^^ { case a @ LITER(_) => LiterExpr(a) }

    lazy val preUnaryExpr: PackratParser[Expression] = preOp ~ identExpr ^^ {
      case pop ~ e2 => PreUnaryExpr(pop, e2)
    }

    lazy val postUnaryExpr: PackratParser[Expression] = identExpr ~ postOp ^^ {
      case e1 ~ pop => PostUnaryExpr(e1, pop)
    }

    lazy val fxnExpr: PackratParser[Expression] = {
      identifier ~ BRACKET(ROUND(true)) ~
      expression ~ BRACKET(ROUND(false)) ^^ {
        case ident ~ _ ~ exp ~ _ => FxnExpr(ident, exp)
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
    def getPairParser(opP: => Parser[BinaryOp]): Parser[ExprOp] = {
      expression ~ opP ^^ {
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

    def prio1Expr: Parser[Expression] = positioned {
      rep(getPairParser(prio1op | prio2op | prio3op | prio4op)) ~
      (prio2Expr | prio3Expr | prio4Expr | prio5Expr) ^^ {
        case lis ~ id => getTreeFromExprList(lis, id)
      }
    }

    def prio2Expr: Parser[Expression] = positioned {
      rep(getPairParser(
        prio2op | prio3op | prio4op)) ~ (prio3Expr | prio4Expr | prio5Expr) ^^ {
        case lis ~ id => getTreeFromExprList(lis, id)
      }
    }

    def prio3Expr: Parser[Expression] = positioned {
      rep(getPairParser(prio3op | prio4op)) ~ (prio4Expr | prio5Expr) ^^ {
        case lis ~ id => getTreeFromExprList(lis, id)
      }
    }

    def prio4Expr: Parser[Expression] = positioned {
      rep(getPairParser(prio4op)) ~ prio5Expr ^^ {
        case lis ~ id => getTreeFromExprList(lis, id)
      }
    }

    def prio5Expr: Parser[Expression] = positioned {
      fxnExpr | bracketExpr |
      identExpr | literExpr |
      preUnaryExpr | postUnaryExpr
    }

    rep(getPairParser(prio1op)) ~ prio1Expr ^^ {
      case lis ~ id => getTreeFromExprList(lis, id)
    }
  }

  // Helpers
  lazy val identifier: PackratParser[IDENT] = positioned {
    accept("identifier", { case id @ IDENT(name) => id })
  }

  lazy val literal: PackratParser[LITER] = positioned {
    accept("string literal", { case lit @ LITER(name) => lit })
  }

  def typeparse: Parser[QualifiedType] = positioned {

    def typename: Parser[CType] = {
      accept("type name", { case TYPE(ctype) => ctype })
    }

    def typenameFromIdent: Parser[CType] = {
      identifier ^^ {
        case IDENT(id) => CUSTOMTYPE(id)
      }
    }

    def typequalifiers: Parser[TYPEQ] = {
      accept("type qualifier", { case tyq @ TYPEQ(quals) => tyq })
    }

    rep(typequalifiers) ~ (typename | typenameFromIdent) ^^ {
      case tql ~ ct => QualifiedType(tql.map(_.qualifier), ct)
    }
  }
}
