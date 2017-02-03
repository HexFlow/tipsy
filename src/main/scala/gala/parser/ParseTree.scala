package gala.parser

import gala.lexer._
import scala.util.parsing.input.Positional

case class QualifiedType(qualifiers: List[String], name: CType) extends ParseTree

sealed trait ParseTree extends Positional

case class DefinitionList(def1: ParseTree, def2: ParseTree) extends ParseTree
case class Definition(qt: QualifiedType, name: String) extends ParseTree

case class FunctionDefinition(
  qt: QualifiedType,
  name: String,
  defs: List[ParseTree]) extends ParseTree
