package gala.lexer

import gala.compiler.{Location, CLexerError}

import scala.util.parsing.combinator.RegexParsers

object CLexer extends RegexParsers {
  override def skipWhitespace = true

  // Handles comments as well as whitespace
  override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def apply(code: String): Either[CLexerError, List[CToken]] = {
    // parse function from RegexParsers
    parse(tokens, code) match {
      case NoSuccess(msg, next) =>
        Left(CLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def tokens: Parser[List[CToken]] = {
    phrase {
      rep1 {
        keyword | ctype | ctypequalifier |
        iconstant | fconstant |
        semi | bracket |
        identifier | literal | operator
      }
    }
  }

  def keyword: Parser[KEYWORD] = positioned {
    ("auto|break|case|char|const|continue|default|do|double|else|enum|extern" |
      "for|goto|if|register|return|signed|sizeof|static" |
      "struct|switch|typedef|union|unsigned|void|volatile|while") ^^ {
      KEYWORD(_)
    }
  }

  def ctypequalifier: Parser[TYPEQ] = positioned {
    ("static|volatile|unsigned|signed|const".r) ^^ {
      TYPEQ(_)
    }
  }

  def ctype: Parser[TYPE] = positioned {
    ("int|byte|short|long|long long|float|double".r) ^^ {
        _ match {
          case "int" => TYPE(INT())
          case "byte" => TYPE(BYTE())
          case "short" => TYPE(SHORT())
          case "long" => TYPE(LONG())
          case "long long" => TYPE(LONGLONG())
          case "float" => TYPE(FLOAT())
          case "double" => TYPE(DOUBLE())
          case "string" => TYPE(STRING())
        }
    }
  }

  def iconstant: Parser[ICONSTANT] = positioned {
    (hex | octal | integer) ^^ { ICONSTANT(_) }
  }

  def fconstant: Parser[FCONSTANT] = positioned {
    (integer ~ "." ~ integer) ^^ {
      case (x ~ "." ~ y) =>
        FCONSTANT((x.toString ++ "." ++ y.toString).toDouble)
    }
  }

  def operator: Parser[OPERATOR] = positioned {
    ("==|!=|>=|<=|&&|[|]{2}|[+]{2}|--|[+]=|-=|[*]=|/=".r |
      "[[(][)]+-[*]/=&|^%<>?]".r) ^^ {
      OPERATOR(_)
    }
  }

  def identifier: Parser[IDENT] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENT(str) }
  }

  def literal: Parser[LITER] = positioned {
    """"[^"]*"""".r ^^ { str =>
      LITER(str.substring(1, str.length - 1))
    }
  }

  def semi: Parser[SEMI] = positioned { ";".r ^^ { _ => SEMI() } }

  def bracket: Parser[BRACKET] = positioned {
    (obracket | cbracket) ^^ { BRACKET(_) }
  }

  // Helper parsers here
  // ===================

  def integer: Parser[Int] = {
    "[0-9]+".r ^^ { x => x.toInt }
  }

  def hex: Parser[Int] = {
    "0[xX][a-fA-F0-9]+".r ^^ {
      s => Integer.parseInt(s.substring(2), 16)
    }
  }

  def octal: Parser[Int] = {
    "0[oO][0-7]+".r ^^ {
      s => Integer.parseInt(s.substring(2), 8)
    }
  }

  def obracket: Parser[CBracket] = {
    """\[|\(|\{""".r ^^ {
      case "{" => CURLY(true)
      case "(" => ROUND(true)
      case "[" => SQUARE(true)
    }
  }

  def cbracket: Parser[CBracket] = {
    """\]|\)|\}""".r ^^ {
      case "}" => CURLY(false)
      case ")" => ROUND(false)
      case "]" => SQUARE(false)
    }
  }
}
