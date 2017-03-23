package tipsy.parser

import tipsy.compiler.{Location, CParserError}
import tipsy.lexer._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

trait Helpers extends PackratParsers with Parsers {

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
    accept("literal", { case lit @ LITER(_) => lit })
  }

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

  // Useful when definitions have to be condensed into a list of individual defs
  def customFlatten(k: List[ParseTree]): List[ParseTree] = {
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
