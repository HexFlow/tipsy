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
      case x => TopList(customFlatten(x))
    })
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

  lazy val definitions: PackratParser[Definitions] = positioned {
    typeparse ~ expression ~ SEMI() ^^ {
      case ty ~ defs ~ _ => {

        val defList = defs match {
          case CompoundExpr(defList) => defList
          case defn => List(defn)
        }

        Definitions(
          defList.collect {
            case BinaryExpr(id, BinaryOp("="), value) =>
              Definition(ty, id, Some(value))
            case id @ (_: IdentExpr | _: PreUnaryExpr | _: ArrayExpr) =>
              Definition(ty, id, None)
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
            IDENT("main")), args, Some(block))
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
    * Expression Statement is an expression followed by a Semi colon.
    */
  def expressionStmt: PackratParser[Expression] = positioned {
    expression ~ SEMI() ^^ { case expr ~ _ => expr }
  }
}
