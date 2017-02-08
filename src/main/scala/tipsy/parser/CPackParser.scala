package tipsy.parser

import tipsy.compiler.{Location, CParserError}
import tipsy.lexer._

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object CPackParser extends Parsers with PackratParsers {
  override type Elem = CToken

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

  lazy val program: PackratParser[ParseTree] = positioned {
    // Program comprises of statementList
    phrase(statementList)
  }

  lazy val statementList: PackratParser[ParseTree] = positioned {
    // It has a lot of statements
    rep1(globalDefinitions) ^^ { case x => x reduceRight DefinitionList }
  }

  lazy val globalDefinitions: PackratParser[ParseTree] = positioned {
    // Statement may be a function or a regular definition
    expression ~ SEMI() ^^ {
      case ex ~ _ => ex
    }
  }

  // Returns an expression object
  lazy val identExpr: PackratParser[Expression] = positioned {
    identifier ^^ {
      case a @ IDENT(_) => IdentExpr(a)
    }
  }

  lazy val expression: PackratParser[Expression] = positioned {
    rep(expression ~ operator) ~ identExpr ^^ {
      case lis ~ id => {
        println(lis)
        println(id)
        id
      }
    }
  }

  lazy val operator: PackratParser[BinaryOp] = positioned {
    accept("operator", { case OPERATOR(bop @ BinaryOp(_)) => bop })
  }

  // Helpers
  lazy val identifier: PackratParser[IDENT] = positioned {
    accept("identifier", { case id @ IDENT(name) => id })
  }

  lazy val literal: PackratParser[LITER] = positioned {
    accept("string literal", { case lit @ LITER(name) => lit })
  }
}
