package tipsy.parser

import tipsy.compiler.{Location, CParserError}
import tipsy.lexer._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.PackratParsers
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
    rep1(functionDefinition | definitions) ^^ {
      case x => TopList(customFlatten(x))
    }
  }

  lazy val blockparser: PackratParser[BlockList] = positioned {

    // A block may have definitions or statements
    rep(expressionStmt | statement | definitions) ^^ {
      case x => BlockList(customFlatten(x))
    }
  }

  lazy val maybeWithoutBracesBlock: PackratParser[BlockList] = positioned {
    val withoutBraces = (expressionStmt | statement | definitions) ^^ {
      case x => BlockList(customFlatten(List(x)))
    }

    val withBraces =
      BRACKET(CURLY(true)) ~ blockparser ~ BRACKET(CURLY(false)) ^^ {
        case _ ~ body ~ _ => body
      }

    withoutBraces | withBraces
  }

  // TODO: Does not support: int a, b = 0;
  lazy val definitions: PackratParser[Definitions] = positioned {
    // A definition may be for an uninitialized object
    val uninitialized = {
      identifier ^^ {
        case id => {
          (id, None)
        }
      }
    }

    // Or an initialized one
    val initialized = {
      identifier ~
      OPERATOR(BinaryOp("=")) ~
      expression ^^ {

        case id ~ _ ~ expr => {
          (id, Some(expr))
        }
      }
    }

    typeparse ~ repsep((initialized | uninitialized), COMMA()) ~ SEMI() ^^ {
      case ty ~ defs ~ _ => {
        Definitions(defs.map { defi => Definition(ty, defi._1, defi._2) })
      }
    }
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
          FxnDefinition(tid, args, Some(block))
        }
      }
    }

    val main = {
      IDENT("main") ~
      BRACKET(ROUND(true)) ~
      repsep(typedvariable, COMMA()) ~
      BRACKET(ROUND(false)) ~
      BRACKET(CURLY(true)) ~
      blockparser ~
      BRACKET(CURLY(false)) ^^ {
        case _ ~ _ ~ args ~_ ~ _ ~ block ~ _ => {
          FxnDefinition(TypedIdent(QualifiedType(List(), INT()), IDENT("main")),
            args, Some(block))
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
          FxnDefinition(tid, args.map(_._1), None)
        }
      }
    }

    main | initialized | uninitialized
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

    lazy val whilestatement: PackratParser[WhileStatement] = {
      WHILE() ~
      BRACKET(ROUND(true)) ~
      expression ~
      BRACKET(ROUND(false)) ~
      maybeWithoutBracesBlock ^^ {
        case _ ~ _ ~ cond ~ _ ~ body => WhileStatement(cond, body)
      }
    }

    lazy val dowhilestatement: PackratParser[DoWhileStatement] = {
      DO() ~
      maybeWithoutBracesBlock ~
      WHILE() ~
      BRACKET(ROUND(true)) ~
      expression ~
      BRACKET(ROUND(false)) ~
      SEMI() ^^ {
        case _ ~ body ~ _ ~ _ ~ cond ~ _ ~ _ => DoWhileStatement(body, cond)
      }
    }

    lazy val returnstatement: PackratParser[ReturnStatement] = {
      KEYWORD("return") ~ expression.? ~ SEMI() ^^ {
        case _ ~ e ~ _ =>
          ReturnStatement(e.getOrElse(LiterExpr(IntLiteral(0))))
      }
    }

    returnstatement | ifstmt | forstmt | whilestatement | dowhilestatement
  }

  /**
    * Expression is a general construct which has a return value.
    * It does not have a semi colon at the end.
    */
  lazy val expression: ExprParse = positioned {

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
      fxnExpr |
      bracketExpr | arrayExpr |
      preUnaryExpr | postUnaryExpr |
      identExpr | literExpr
    }

    lazy val assignExpr: PackratParser[AssignExpr] = {
      (preUnaryExpr | postUnaryExpr | arrayExpr | identExpr) ~
      OPERATOR(BinaryOp("=")) ~ simpleExpr ^^ {
        case eid ~ op ~ expr => AssignExpr(eid, expr)
      }
    }

    lazy val simpleExpr: ExprParse = assignExpr | prio1Expr

    lazy val compoundExpr: ExprParse = {
      repsep(simpleExpr, COMMA()) ^^ {
        case expr :: Nil => expr
        case exprs => CompoundExpr(exprs)
      }
    }

    log(compoundExpr)("Logger")
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

  // Useful when definitions have to be condensed into a list of individual defs
  private def customFlatten(k: List[ParseTree]): List[ParseTree] = {
    k match {
      case Nil => List()
      case x :: xs => {
        x match {
          case Definitions(defs) => defs ++ customFlatten(xs)
          case y => y :: customFlatten(xs)
        }
      }
    }
  }
}
