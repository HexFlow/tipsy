package tipsy.lexer

import tipsy.compiler.{Location, CLexerError}

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
        keyword | ctype |
        elsef | iff |
        forf | whilef | dof |
        ctypequalifier | semi | comma | bracket |
        identifier | literal | operator
      }
    }
  }

  def keyword: Parser[KEYWORD] = positioned {
    ("auto|break|case|continue|default|enum|extern" |
      "goto|register|return|sizeof" |
      "struct|switch|typedef|union") ^^ {
      KEYWORD(_)
    }
  }

  def ctypequalifier: Parser[TYPEQ] = positioned {
    ("static|volatile|unsigned|signed|const".r) ^^ {
      TYPEQ(_)
    }
  }

  def ctype: Parser[TYPE] = positioned {
    ("int|char|byte|short|long|long long|float|double|void".r) ^^ {
        _ match {
          case "int" => TYPE(INT())
          case "byte" => TYPE(BYTE())
          case "char" => TYPE(CHAR())
          case "short" => TYPE(SHORT())
          case "long" => TYPE(LONG())
          case "long long" => TYPE(LONGLONG())
          case "float" => TYPE(FLOAT())
          case "double" => TYPE(DOUBLE())
          case x => TYPE(CUSTOMTYPE(x))
        }
    }
  }

  def operator: Parser[OPERATOR] = positioned {
    val binaryOp = """[+*/-]|>=|>|<=|<|==|!=|[|]{1,2}|&{1,2}|\+=|\*=|=""".r ^^ {
      case x @ ("=") => OPERATOR(BinaryOp("="))
      case x @ (">=" | "<=" | ">" | "<" | "!=" | "==") =>
        OPERATOR(ParseBinaryOp(Prio1(x)))
      case x @ ("*" | "/") => OPERATOR(ParseBinaryOp(Prio2(x)))
      case x @ ("+" | "-") => OPERATOR(ParseBinaryOp(Prio3(x)))
      case x => OPERATOR(ParseBinaryOp(Prio4(x)))
    }

    val preUnaryOp = """[+]{2}|-{2}|!|&""".r ^^ {
      case x => OPERATOR(PreUnaryOp(x))
    }

    val postUnaryOp = """[+]{2}|-{2}""".r ^^ {
      case x => OPERATOR(PostUnaryOp(x))
    }

    postUnaryOp | preUnaryOp | binaryOp
  }

  def identifier: Parser[IDENT] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENT(str) }
  }

  def literal: Parser[LITER] = positioned {
    (iliteral | fliteral | sliteral) ^^ { LITER(_) }
  }

  def semi: Parser[SEMI] = positioned { ";".r ^^ { _ => SEMI() } }

  def comma: Parser[COMMA] = positioned { ",".r ^^ { _ => COMMA() } }

  def iff: Parser[IF] = positioned { "if".r ^^ { _ => IF() } }
  def elsef: Parser[ELSE] = positioned { "else".r ^^ { _ => ELSE() } }
  def forf: Parser[FOR] = positioned { "for".r ^^ { _ => FOR() }}
  def whilef: Parser[WHILE] = positioned { "while".r ^^ { _ => WHILE() }}
  def dof: Parser[DO] = positioned { "do".r ^^ { _ => DO() }}

  def bracket: Parser[BRACKET] = positioned {
    (obracket | cbracket) ^^ { BRACKET(_) }
  }

  // Helper parsers here =>
  // ------------------- =>

  def iliteral: Parser[IntLiteral] = positioned {
    (hex | octal | integer | binary) ^^ { IntLiteral(_) }
  }

  def fliteral: Parser[FloatLiteral] = positioned {
    (integer ~ "." ~ integer) ^^ {
      case (x ~ "." ~ y) =>
        FloatLiteral((x.toString ++ "." ++ y.toString).toDouble)
    }
  }

  def sliteral: Parser[StrLiteral] = positioned {
    """"[^"]*"""".r ^^ { str =>
      StrLiteral(str.substring(1, str.length - 1))
    }
  }

  def integer: Parser[Int] = {
    "[0-9]+".r ^^ { x => x.toInt }
  }

  def hex: Parser[Int] = {
    "0[xX][a-fA-F0-9]+".r ^^ {
      s => Integer.parseInt(s.substring(2), 16)
    }
  }

  def binary: Parser[Int] = {
    "0[bB][0-1]+".r ^^ {
      s => Integer.parseInt(s.substring(2), 2)
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
