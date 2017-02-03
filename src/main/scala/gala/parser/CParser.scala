package gala.parser

import gala.compiler.{Location, CParserError}
import gala.lexer._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object CParser extends Parsers {
  override type Elem = CToken

  class CTokenReader(tokens: Seq[Elem]) extends Reader[Elem] {
    override def first: Elem = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest: Reader[Elem] = new CTokenReader(tokens.tail)
  }


  def apply(tokens: Seq[Elem]): Either[CParserError, ParseTree] = {
    val reader = new CTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) =>
        Left(CParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def program: Parser[ParseTree] = positioned {
    // Program comprises of statementList
    phrase(statementList)
  }

  def statementList: Parser[ParseTree] = positioned {
    // It has a lot of statements
    rep1(globalDefinitions) ^^ { case x => x reduceRight DefinitionList }
  }

  def globalDefinitions: Parser[ParseTree] = positioned {
    // Statement may be a function or a regular definition
    functionDefinition | definition
  }

  def definition: Parser[ParseTree] = positioned {
    val uninitialized = typeparse ~ identifier ~ SEMI() ^^ {
      case qualifiedType ~ IDENT(id) ~ _ =>
        UnDefinition(qualifiedType, id)
    }

    val initialized = {
      typeparse ~ identifier ~
      OPERATOR(StatementOp("=")) ~
      expression ~ SEMI() ^^ {

        case qualifiedType ~ IDENT(id) ~ _ ~ expr ~ _ =>
          Definition(qualifiedType, id, expr)
      }}

    // A regular definition will either be uninitialized, or initialized
    uninitialized | initialized
  }

  def functionDefinition: Parser[ParseTree] = positioned {
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

  def statement: Parser[ParseTree] = positioned {
    // Returns a complete statement
    identifier ~ operator("stmt") ~ expression ~ SEMI() ^^ {
      case id ~ op ~ expr ~ _ => Statement(id, op, expr)
    }
  }

  def expression: Parser[Expression] = positioned {
    // Returns an expression object
    val identExpr = identifier ^^ { case a @ IDENT(_) => IdentExpr(a) }
    val literExpr = literal ^^ { case a @ LITER(_) => LiterExpr(a) }

    val binaryExpr = (identExpr | literExpr) ~ operator("bin") ~ expression ^^ {
      case e1 ~ OPERATOR(op) ~ e2 => {
        op match { // Ignore non-binary operators in this match
          case a @ BinaryOp(_) => BinaryExpr(e1, a, e2)
        }
      }
    }

    val preUnaryExpr = operator("pre") ~ expression ^^ {
      case OPERATOR(op) ~ e2 => {
        op match {
          case a @ PreUnaryOp(_) => PreUnaryExpr(a, e2)
        }
      }
    }

    val postUnaryExpr = (identExpr | literExpr) ~ operator("post") ^^ {
      case e1 ~ OPERATOR(op) => {
        op match {
          case a @ PostUnaryOp(_) => PostUnaryExpr(e1, a)
        }
      }
    }

    postUnaryExpr | preUnaryExpr | binaryExpr | identExpr | literExpr
  }

  // Helpers
  private def identifier: Parser[IDENT] = positioned {
    accept("identifier", { case id @ IDENT(name) => id })
  }

  private def literal: Parser[LITER] = positioned {
    accept("string literal", { case lit @ LITER(name) => lit })
  }

  // TODO Make this more generic to gain compile time guarantees
  private def operator(style: String): Parser[OPERATOR] = positioned {
    accept("operator", {
      case op @ OPERATOR(BinaryOp(_)) if style == "bin" => op
      case op @ OPERATOR(PreUnaryOp(_)) if style == "pre" => op
      case op @ OPERATOR(PostUnaryOp(_)) if style == "post" => op
      case op @ OPERATOR(StatementOp(_)) if style == "stmt" => op
    })
  }

  private def typeparse: Parser[QualifiedType] = positioned {

    def typename: Parser[TYPE] = {
      accept("type", { case ty @ TYPE(ctype) => ty })
    }

    def typequalifiers: Parser[TYPEQ] = {
      accept("type qualifier", { case tyq @ TYPEQ(quals) => tyq })
    }

    rep(typequalifiers) ~ typename ^^ {
      case tql ~ TYPE(ct) => QualifiedType(tql.map(_.qualifier), ct)
    }
  }
}
