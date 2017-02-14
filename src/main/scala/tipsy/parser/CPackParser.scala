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
    phrase(programparser)
  }

  lazy val programparser: PackratParser[TopList] = positioned {
    // Program may have functions or other definitions/declarations
    rep1(functionDefinition | definition) ^^ {
      case x => TopList(x)
    }
  }

  lazy val blockparser: PackratParser[BlockList] = positioned {
    // A block may have definitions or statements
    rep(expressionStmt | statement | definition) ^^ {
      case x => BlockList(x)
    }
  }

  lazy val maybeWithoutBracesBlock: PackratParser[BlockList] = positioned {
    val withoutBraces = (expressionStmt | statement | definition) ^^ {
      case x => BlockList(List(x))
    }

    val withBraces =
      BRACKET(CURLY(true)) ~ blockparser ~ BRACKET(CURLY(false)) ^^ {
        case _ ~ body ~ _ => body
      }

    withoutBraces | withBraces
  }

  lazy val definition: PackratParser[Definition] = positioned {
    // A definition may be for an uninitialized object
    val uninitialized = {
      typedvariable ~ SEMI() ^^ {
        case tid ~ _ => {
          Uninitialized(tid)
        }
      }
    }

    // Or an initialized one
    val initialized = {
      typedvariable ~
      OPERATOR(BinaryOp("=")) ~
      expressionStmt ^^ {

        case tid ~ _ ~ expr => {
          Initialized(tid, expr)
        }
      }
    }

    uninitialized | initialized
  }

  lazy val functionDefinition: PackratParser[FxnDefinition] = positioned {
    // Similarly, a function may be defined at the same place
    val initialized = {
      typedvariable ~
      BRACKET(ROUND(true)) ~
      repsep(typedvariable, COMMA()) ~
      BRACKET(ROUND(false)) ~
      BRACKET(CURLY(true)) ~
      blockparser ~
      BRACKET(CURLY(false)) ^^ {

        case tid ~ _ ~ args ~_ ~ _ ~ block ~ _ => {
          InitializedFxn(tid, args, block)
        }
      }
    }

    // Or just declared here
    val uninitialized = {
      typedvariable ~
      BRACKET(ROUND(true)) ~
      rep(typedvariable ~ COMMA()) ~
      BRACKET(ROUND(false)) ~ SEMI() ^^ {

        case tid ~ _ ~ args ~_ ~ _ => {
          UninitializedFxn(tid, args.map(_._1))
        }
      }
    }

    initialized | uninitialized
  }

  /**
    * Statements, as opposed to expressions, are self sufficient
    * entities inside a block, and include a semi colon at the end.
    * These do not return a value.
    */
  lazy val statement: TreeParse = positioned {
    // Returns a complete statement

    lazy val ifstmt: PackratParser[IfStatement] = {
      IF() ~
      BRACKET(ROUND(true)) ~
      expression ~
      BRACKET(ROUND(false)) ~
      maybeWithoutBracesBlock ~
      opt(
        ELSE() ~ maybeWithoutBracesBlock
      ) ^^ {
        case _ ~ _ ~ cond ~ _ ~ body ~ elseblk => {
          val ebody = elseblk match {
            case Some(_ ~ elsebody) => elsebody
            case _ => BlockList(List())
          }
          IfStatement(cond, body, ebody)
        }
      }
    }

    lazy val forstmt: PackratParser[ForStatement] = {
      FOR() ~
      BRACKET(ROUND(true)) ~
      expressionStmt ~
      expressionStmt ~
      expression ~
      BRACKET(ROUND(false)) ~
      maybeWithoutBracesBlock ^^ {
        case _ ~ _ ~ s1 ~ s2 ~ s3 ~ _ ~ body => {
          ForStatement(s1, s2, s3, body)
        }
      }
    }

    ifstmt | forstmt
  }

  /**
    * Expression is a general construct which has a return value.
    * It does not have a semi colon at the end.
    */
  lazy val expression: ExprParse = positioned {

    lazy val assignExpr: PackratParser[AssignExpression] = {
      identifier ~ OPERATOR(BinaryOp("=")) ~ expression ^^ {
        case id ~ op ~ expr => AssignExpression(id, expr)
      }
    }

    lazy val identExpr: PackratParser[Expression] =
      identifier ^^ { case a => IdentExpr(a) }

    lazy val literExpr: PackratParser[LiterExpr] =
      literal ^^ { case a @ LITER(_) => LiterExpr(a) }

    lazy val preUnaryExpr: PackratParser[Expression] = unaryOp ~ identExpr ^^ {
      case pop ~ e2 => PreUnaryExpr(pop, e2)
    }

    lazy val postUnaryExpr: PackratParser[Expression] = identExpr ~ unaryOp ^^ {
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
      assignExpr |
      fxnExpr | bracketExpr |
      preUnaryExpr | postUnaryExpr |
      identExpr | literExpr
    }

    // The final expression type definition
    // Prio1 will fallback to other higher priority operators
    rep(getPairParser(prio1op)) ~ prio1Expr ^^ {
      case lis ~ id => getTreeFromExprList(lis, id)
    }
  }

  /**
    * Expression Statement is an expression followed by a Semi colon.
    */
  def expressionStmt: PackratParser[Expression] = positioned {
    expression ~ SEMI() ^^ { case expr ~ _ => expr }
  }

  // Helper functions
  // ================

  def typeparse: Parser[QualifiedType] = positioned {
    rep(typequalifiers) ~ (typename | typenameFromIdent) ^^ {
      case tql ~ ct => QualifiedType(tql.map(_.qualifier), ct)
    }
  }

  def typedvariable: Parser[TypedIdent] = positioned {
    typeparse ~ identifier ^^ {
      case tql ~ id => TypedIdent(tql, id)
    }
  }

  lazy val identifier: PackratParser[IDENT] = positioned {
    accept("identifier", { case id @ IDENT(_) => id })
  }

  lazy val literal: PackratParser[LITER] = positioned {
    accept("string literal", { case lit @ LITER(_) => lit })
  }

  private def typename: Parser[CType] = {
    accept("type name", { case TYPE(ctype) => ctype })
  }

  private def typenameFromIdent: Parser[CType] = {
    identifier ^^ {
      case IDENT(id) => CUSTOMTYPE(id)
    }
  }

  private def typequalifiers: Parser[TYPEQ] = {
    accept("type qualifier", { case tyq @ TYPEQ(quals) => tyq })
  }
}
