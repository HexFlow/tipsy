package tipsy.parser

import tipsy.compiler.{Location, CParserError}
import tipsy.lexer._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object CPackParser extends PackratParsers with Parsers
    with OperatorParsers with Helpers with ExprParse {

  override type Elem = CToken

  type TreeParse = PackratParser[ParseTree]

  class CTokenReader(tokens: Seq[Elem]) extends Reader[Elem] {
    override def first: Elem = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position =
      tokens.headOption.map(_.pos).getOrElse(NoPosition)
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
    // Program may have functions or other definitions/declarations
    phrase(rep1(functionDefinition | definitions) ^^ {
      case x => TopList(sortFunctionsInUseOrder(customFlatten(x)))
    })
  }

  lazy val blockparser: PackratParser[BlockList] = positioned {
    // A block may have definitions or statements
    rep(expressionStmt | statement | definitions) ^^ {
      case x => BlockList(customFlatten(x))
    }
  }

  lazy val loopBlockparser: PackratParser[BlockList] = positioned {
    //loops has break and continue too.
    rep(flowifstmt | flowStmt | expressionStmt | statement | definitions) ^^ {
      case x => BlockList(customFlatten(x))
    }
  }

  lazy val flowifstmt: PackratParser[IfStatement] = {
    IF() ~
    BRACKET(ROUND(true)) ~
    expression ~
    BRACKET(ROUND(false)) ~
    maybeWithoutBracesLoopBlock ~
    opt(
      ELSE() ~ maybeWithoutBracesLoopBlock
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

  lazy val flowStmt: PackratParser[FlowStatement] = positioned {
    lazy val br: PackratParser[Break] = {
      KEYWORD("break") ~ SEMI() ^^ {
        case b ~ _ => Break()
      }
    }

    lazy val con: PackratParser[Continue] = {
      KEYWORD("continue") ~ SEMI() ^^ {
        case c ~ _ => Continue()
      }
    }

    br | con
  }

  lazy val maybeWithoutBracesSwitchBlock: PackratParser[BlockList] = positioned {
    val withoutBraces = loopBlockparser ^^ {
      case body => body
    }
    val withBraces = {
      BRACKET(CURLY(true)) ~ loopBlockparser ~ BRACKET(CURLY(false)) ^^ {
        case _ ~ body ~ _ => body
      }
    }
    withoutBraces | withBraces
  }

  lazy val maybeWithoutBracesLoopBlock: PackratParser[BlockList] = positioned {
    val withoutBraces = (flowifstmt | flowStmt | expressionStmt | statement | definitions) ^^ {
      case x => BlockList(customFlatten(List(x)))
    }

    val withBraces = {
      BRACKET(CURLY(true)) ~ loopBlockparser ~ BRACKET(CURLY(false)) ^^ {
        case _ ~ body ~ _ => body
      }
    }

    withoutBraces | withBraces
  }

  lazy val definitions: PackratParser[Definitions] = positioned {
    typeparse ~ expression.? ~ SEMI() ^^ {
      case (qt @ QualifiedType(List(), CUSTOMTYPE("void"))) ~ None ~ _ => Definitions(List(Definition(qt, None, None)))
      case ty ~ Some(defs) ~ _ => {

        val defList = defs match {
          case CompoundExpr(defList) => defList
          case defn => List(defn)
        }

        Definitions(
          defList.collect {
            case BinaryExpr(id, BinaryOp("="), value) =>
              Definition(ty, Some(id), Some(value))
            case id @ (_: IdentExpr | _: PreUnaryExpr | _: ArrayExpr) =>
              Definition(ty, Some(id), None)
        })
      }
    }
  }

  lazy val functionDefinition: PackratParser[FxnDefinition] = positioned {
    val signature = {
      BRACKET(ROUND(true)) ~
      repsep(typedvariable, comma) ~
      BRACKET(ROUND(false)) ^^ {
        case _ ~ b ~ _ => b
      }
    }

    // Similarly, a function may be defined at the same place
    val initialized = {
      typedvariable ~
      signature ~
      BRACKET(CURLY(true)) ~
      blockparser ~
      BRACKET(CURLY(false)) ^^ {

        case ret ~ args ~ _ ~ block ~ _ => {
          FxnDefinition(ret, args, Some(block))
        }
      }
    }

    val main = {
      IDENT("main") ~
      signature ~
      BRACKET(CURLY(true)) ~
      blockparser ~
      BRACKET(CURLY(false)) ^^ {
        case _ ~ args ~ _ ~ block ~ _ => {
          FxnDefinition(TypedIdent(QualifiedType(List(), INT()),
            Some(IDENT("main"))), args, Some(block))
        }
      }
    }

    // Or just declared here
    val uninitialized = {
      typedvariable ~ signature ~ SEMI() ^^ {
        case tid ~ args ~ _ => {
          FxnDefinition(tid, args, None)
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

    lazy val switchStatement: PackratParser[SwitchStatement] = {
      SWITCH() ~
      BRACKET(ROUND(true)) ~
      expression ~
      BRACKET(ROUND(false)) ~
      BRACKET(CURLY(true)) ~
      rep(KEYWORD("case") ~ expression ~ COLON() ~ maybeWithoutBracesSwitchBlock) ~
      opt(KEYWORD("default") ~ COLON() ~ maybeWithoutBracesSwitchBlock) ~
      rep(KEYWORD("case") ~ expression ~ COLON() ~ maybeWithoutBracesSwitchBlock) ~
      BRACKET(CURLY(false)) ^^ {
        case _ ~ _ ~ expr ~ _ ~ _ ~ caseblk1 ~ defaultblk ~ caseblk2 ~ _ => {
          val defaultBody =  defaultblk match {
            case Some(_ ~ _ ~ dbody) => dbody
            case _ => BlockList(List())
          }
          val caseBody1 = caseblk1.map {
            x => x match {
              case (_ ~ exp ~ _ ~ cbody) => (exp, cbody)
            }
          }
          val caseBody2 = caseblk2.map {
            x => x match {
              case (_ ~ exp ~ _ ~ cbody) => (exp, cbody)
            }
          }
          SwitchStatement(expr, caseBody1 ++ caseBody2, defaultBody)
        }
      }
    }

    lazy val forstmt: PackratParser[ForStatement] = {
      FOR() ~
      BRACKET(ROUND(true)) ~
      expressionStmt ~
      expressionStmt ~
      expression.? ~
      BRACKET(ROUND(false)) ~
      maybeWithoutBracesLoopBlock ^^ {
        case _ ~ _ ~ s1 ~ s2 ~ None ~ _ ~ body => {
          ForStatement(s1, s2, CompoundExpr(List(): List[Expression]), body)
        }
        case _ ~ _ ~ s1 ~ s2 ~ Some(s3) ~ _ ~ body => {
          ForStatement(s1, s2, s3, body)
        }
      }
    }

    lazy val whilestatement: PackratParser[WhileStatement] = {
      WHILE() ~
      BRACKET(ROUND(true)) ~
      expression ~
      BRACKET(ROUND(false)) ~
      maybeWithoutBracesLoopBlock ^^ {
        case _ ~ _ ~ cond ~ _ ~ body => WhileStatement(cond, body)
      }
    }

    lazy val dowhilestatement: PackratParser[DoWhileStatement] = {
      DO() ~
      maybeWithoutBracesLoopBlock ~
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
 
    returnstatement | ifstmt | switchStatement | forstmt | whilestatement | dowhilestatement
  }

  /**
    * Expression Statement is an expression followed by a Semi colon.
    */
  def expressionStmt: PackratParser[Expression] = positioned {
    expression.? ~ SEMI() ^^ {
      case None ~ _ => CompoundExpr(List(): List[Expression])
      case Some(expr) ~ _ => expr
    }
  }
}
