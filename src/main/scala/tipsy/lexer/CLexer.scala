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
      case Success(result, next) => {
        /*Right(result.foldRight(List(): List[CToken]) {
          (x, acc) => x match{
            case OPERATOR(CompoundOp(_, x :: y :: Nil)) => OPERATOR(x) :: OPERATOR(y) :: acc
            case x => x :: acc
          }
        })*/
       Right(result)
      }
    }
  }

  def tokens: Parser[List[CToken]] = {
    phrase {
      rep1 {
        keyword | ctype |
        elsef | iff | switchf |
        forf | whilef | dof | question |
        ctypequalifier | semi | colon | bracket |
        identifier | literal | operator
      }
    }
  }

  def keyword: Parser[KEYWORD] = positioned {
    ("auto|break|case|continue|default|enum|extern".r |
      "goto|return|register|sizeof".r |
      "struct|typedef|union".r) ^^ {
      KEYWORD(_)
    }
  }

  def ctypequalifier: Parser[TYPEQ] = positioned {
    ("static|volatile|unsigned|signed|const".r) ^^ {
      TYPEQ(_)
    }
  }

  def ctype: Parser[TYPE] = positioned {
    ("(int|char|byte|short|long long int|long long|long int|long|float|double|void)\\b".r) ^^ {
        _ match {
          case "int" => TYPE(INT())
          case "byte" => TYPE(BYTE())
          case "char" => TYPE(CHAR())
          case "short" => TYPE(SHORT())
          case "long long int" => TYPE(LONGLONG())
          case "long long" => TYPE(LONGLONG())
          case "long int" => TYPE(LONG())
          case "long" => TYPE(LONG())
          case "float" => TYPE(FLOAT())
          case "double" => TYPE(DOUBLE())
          case x => TYPE(CUSTOMTYPE(x))
        }
    }
  }

  val maxLevel = 15
  def operator: Parser[OPERATOR] = positioned {
    def op(x: String, level: Int) = OPERATOR(ParseBinaryOp(x, level))

    val binaryOp =
      """\,|<<=|>>=|>>|<<|>=|>|<=|<|==|!=|\+=|\*=|\-=|\/=|[+*/-]|\%=|\&=|\^=|\|=|=|%|[|]{1,2}|&{1,2}|\^""".r ^^ {
        case x @ "," => op(x, 1)

        case x @ ("=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "^=" | "|=" |
            "<<=" | ">>=") => op(x, 2)

        case x @ "?:" => op(x, 3)

        case x @ "||" => op(x, 4)

        case x @ "&&" => op(x, 5)

        case x @ "|" => op(x, 6)

        case x @ "^" => op(x, 7)

        case x @ "&" => op(x, 8)

        case x @ ("==" | "!=") => op(x, 9)

        case x @ ("<=" | "<" | ">=" | ">") => op(x, 10)

        case x @ ("<<" | ">>") => op(x, 11)

        case x @ ("+" | "-") => op(x, 12)

        case x @ ("*" | "%") => op(x, 13)

        case x => op(x, 14)
    }

    val unaryOp = """[+]{2}|-{2}""".r ^^ {
      case x => OPERATOR(UnaryOp(x))
    }

    val unaryOp1 = """!|~""".r ^^ {
      case x => OPERATOR(UnaryOp(x))
    }

    val signOp = binaryOp ~ """\+|\-""".r ^^ {
      case OPERATOR(x) ~ y => OPERATOR(CompoundOp("Compound Operation", List(x, UnaryOp(y))))
    }

    //unaryOp | signOp | binaryOp | unaryOp1
     unaryOp | binaryOp | unaryOp1
  }

  def identifier: Parser[IDENT] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENT(str) }
  }

  def literal: Parser[LITER] = positioned {
    (fliteral | iliteral | sliteral) ^^ { LITER(_) }
  }

  def semi: Parser[SEMI] = positioned { ";".r ^^ { _ => SEMI() } }
  def colon: Parser[COLON] = positioned { ":".r ^^ { _ => COLON() } }
  def question: Parser[QUESTION] = positioned { "\\?".r ^^ { _ => QUESTION() }}
  // def comma: Parser[COMMA] = positioned { ",".r ^^ { _ => COMMA() } }

  def iff: Parser[IF] = positioned { "if".r ^^ { _ => IF() } }
  def switchf: Parser[SWITCH] = positioned { "switch".r ^^ { _ => SWITCH() } }
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
    (integer.? ~ "." ~ integer) ^^ {
      case (x ~ "." ~ y) =>
        FloatLiteral((x.getOrElse(0).toString ++ "." ++ y.toString).toDouble)
    }
  }

  def sliteral: Parser[StrLiteral] = positioned {
    """("(\\.|[^\\"])*"|'.'|'\\.')""".r ^^ { str =>
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
    "0[oO]?[0-7]+".r ^^ {
      case s if (s.substring(1, 2) == "o" || s.substring(1, 2) == "O") => Integer.parseInt(s.substring(2), 8)
      case s => Integer.parseInt(s.substring(1), 8)
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
