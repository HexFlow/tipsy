package tipsy.parser

import tipsy.compiler.{Location, CParserError}
import tipsy.lexer._

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
    identifier ~ stmtOp ~ expression ~ SEMI() ^^ {
      case id ~ op ~ expr ~ _ => Statement(id, op, expr)
    }
  }

  def expression: Parser[Expression] = positioned {
    // Returns an expression object
    val identExpr = identifier ^^ { case a @ IDENT(_) => IdentExpr(a) }
    val literExpr = literal ^^ { case a @ LITER(_) => LiterExpr(a) }

    val binaryExpr = (identExpr | literExpr) ~ binOp ~ expression ^^ {
      case e1 ~ bop ~ e2 => BinaryExpr(e1, bop, e2)
    }

    val preUnaryExpr = preOp ~ identExpr ^^ {
      case pop ~ e2 => PreUnaryExpr(pop, e2)
    }

    val postUnaryExpr = identExpr ~ postOp ^^ {
      case e1 ~ pop => PostUnaryExpr(e1, pop)
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

  private def postOp: Parser[PostUnaryOp] = positioned {
    operator ^^ { case OPERATOR(pop @ PostUnaryOp(_)) => pop }
  }

  private def preOp: Parser[PreUnaryOp] = positioned {
    operator ^^ { case OPERATOR(pop @ PreUnaryOp(_)) => pop }
  }

  private def binOp: Parser[BinaryOp] = positioned {
    operator ^^ { case OPERATOR(bop @ BinaryOp(_)) => bop }
  }

  private def stmtOp: Parser[StatementOp] = positioned {
    operator ^^ { case OPERATOR(sop @ StatementOp(_)) => sop }
  }

  private def operator: Parser[OPERATOR] = positioned {
    accept("operator", {case op @ OPERATOR(_) => op})
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
