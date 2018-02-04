package tipsy.parser

import tipsy.lexer._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.PackratParsers
import scala.collection.mutable.{ Set => mSet }

trait Helpers extends PackratParsers with Parsers {

  override type Elem = CToken

  def typeparse: Parser[QualifiedType] = positioned {
    rep(typequalifiers) ~ (typename | typenameFromIdent) ^^ {
      case tql ~ ct => QualifiedType(tql.map(_.qualifier), ct)
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

  /**
    * Takes a list of block entries (definitions etc). Out of those,
    * condenses the Definitions construct (eg: int a, b = 2;) into
    * a list of individual definitions
    */
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

  /**
    * Takes a list of top level declarations (functions etc).
    * Sorts the function definitions in order of first use and returns
    * all the top level definitions followed by the sorted list of function
    * definitions.
    * Removes functions without bodies
    */
  def sortFunctionsInUseOrder(lis: List[ParseTree]): List[ParseTree] = {
    val defs = lis.collect { case x @ Definition(_, _, _) => x }
    val fdefs = lis
      .collect { case x @ FxnDefinition(ti, _, Some(_)) => ti.name.str -> x }
      .toMap

    val usedFxns: mSet[String] = mSet()

    def filterFuncNames(expr: List[String]): List[String] = {
      expr.filter(_.startsWith("func:")).map(_.drop(5))
    }

    // Takes a function name and provides bodies of functions called by it
    // Takes care not to return functions which have been already returned
    def processFxn(name: String): List[ParseTree] = {
      if (usedFxns contains name) {
        List()
      } else {
        usedFxns add name

        (fdefs get name) match {
          case None => List()
          case Some(fxn @ FxnDefinition(_, _, Some(body))) => {
            val fxns =
              body.compress.collect { case POSTEXPR(e) => e }.flatMap(filterFuncNames(_))
            fxn :: fxns.flatMap(processFxn(_))
          }
          case _ => ???
        }
      }
    }

    defs ++ processFxn("main")
  }

}
