package tipsy.parser

import tipsy.compiler.{Location, CParserError}
import tipsy.lexer._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object CParser extends Parsers with OperatorParsers {
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
      }
    }

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

    val preUnaryExpr = preOp ~ identExpr ^^ {
      case pop ~ e2 => PreUnaryExpr(pop, e2)
    }

    val postUnaryExpr = identExpr ~ postOp ^^ {
      case e1 ~ pop => PostUnaryExpr(e1, pop)
    }

    val fxnExpr = {
      identifier ~ BRACKET(ROUND(true)) ~
      expression ~ BRACKET(ROUND(false)) ^^ {
        case ident ~ _ ~ exp ~ _ => FxnExpr(ident, exp)
      }
    }

    val bracketExpr = {
      BRACKET(ROUND(true)) ~ expression ~ BRACKET(ROUND(false)) ^^ {
        case _ ~ exp ~ _ => exp
      }
    }

    def prio1Expr: Parser[Expression] = positioned {
      (prio2Expr | prio3Expr | prio4Expr | prio5Expr) ~ prio1op ~ expression ^^ {
        case e1 ~ op ~ e2 => BinaryExpr(e1, op, e2)
      }
    }

    def prio2Expr: Parser[Expression] = positioned {
      (prio3Expr | prio4Expr | prio5Expr) ~ prio2op ~ expression ^^ {
        case e1 ~ op ~ e2 => BinaryExpr(e1, op, e2)
      }
    }

    def prio3Expr: Parser[Expression] = positioned {
      (prio4Expr | prio5Expr) ~ prio3op ~ expression ^^ {
        case e1 ~ op ~ e2 => BinaryExpr(e1, op, e2)
      }
    }

    def prio4Expr: Parser[Expression] = positioned {
      prio5Expr ~ prio4op ~ expression ^^ {
        case e1 ~ op ~ e2 => BinaryExpr(e1, op, e2)
      }
    }

    def prio5Expr: Parser[Expression] = positioned {
      fxnExpr | bracketExpr |
      identExpr | literExpr |
      preUnaryExpr | postUnaryExpr
    }

    val prioExprs = List(prio1Expr, prio2Expr, prio3Expr, prio4Expr, prio5Expr)

    // Expression is the OR of all these prioritized expressions
    prioExprs reduceRight ((x, y) => x | y)
  }

  // Helpers
  private def identifier: Parser[IDENT] = positioned {
    accept("identifier", { case id @ IDENT(name) => id })
  }

  private def literal: Parser[LITER] = positioned {
    accept("string literal", { case lit @ LITER(name) => lit })
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
