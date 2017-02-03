package gala.parser

import gala.lexer._
import scala.util.parsing.input.Positional

// Used for storing types
case class QualifiedType(qualifiers: List[String], name: CType) extends ParseTree

// All other ParseTree constructs derive from this
sealed trait ParseTree extends Positional

// ParseTree constructs follow =>
// --------------------------- =>

// List of definitions
case class DefinitionList(def1: ParseTree, def2: ParseTree) extends ParseTree

// An initialized definition
case class Definition(
  qt: QualifiedType,
  name: String,
  value: ParseTree) extends ParseTree

// An uninitialized definition
case class UnDefinition(qt: QualifiedType, name: String) extends ParseTree

// A function with type and definition
case class FunctionDefinition(
  qt: QualifiedType,
  name: String,
  defs: List[ParseTree]) extends ParseTree

// Expression constructs follow =>
// ---------------------------- =>

sealed trait Expression extends ParseTree
case class IdentExpr(id: IDENT) extends Expression
case class LiterExpr(liter: LITER) extends Expression
