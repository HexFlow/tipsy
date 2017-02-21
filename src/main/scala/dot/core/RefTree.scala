package dot.core

import com.softwaremill.quicklens._

import scala.annotation.implicitNotFound
import scala.collection.immutable.CollectionInstances

/**
 * A [[RefTree]] represents the object tree of an immutable data structure.
 * Mutable data can be represented as well, as long as it is acyclic.
 *
 * A [[RefTree]] has three subtypes:
 *  - [[RefTree.Ref]] represents an object ([[AnyRef]]) with identity and a sequence of fields;
 *  - [[RefTree.Val]] represents a value ([[AnyVal]]);
 *  - [[RefTree.Null]] is a special case to represent `null`.
 *
 * This type is mainly geared towards visualization of structural sharing,
 * however in some cases the representation can be simplified for the benefit of the visualization.
 * For example, [[reftree.contrib.SimplifiedInstances]] contains “black box” representations
 * of [[List]], [[Seq]] and [[Map]] that do not expose the internal structure.
 *
 * A [[RefTree]] for a type `A` can be obtained using the [[ToRefTree]] typeclass.
 */
sealed trait RefTree {
  /**
   * The identifier for this tree.
   * Note that this is unique only for [[RefTree.Ref]]. For [[RefTree.Null]] it is always `null`,
   * and value ([[RefTree.Val]]) identifiers are shared across all equivalent values.
   */
  def id: String

  /** Whether this tree should be highlighted during visualization */
  def highlight: Boolean

  /** Whether this tree should be elided during visualization */
  def elide: Boolean

  /** Add or remove highlighting */
  def withHighlight(highlight: Boolean) = this match {
    case tree: RefTree.Val ⇒ tree.copy(highlight = highlight)
    case tree: RefTree.Null ⇒ tree.copy(highlight = highlight)
    case tree: RefTree.Ref ⇒ tree.copy(highlight = highlight)
  }

  /** Set or unset elision */
  def withElide(elide: Boolean) = this match {
    case tree: RefTree.Val ⇒ tree.copy(elide = elide)
    case tree: RefTree.Null ⇒ tree.copy(elide = elide)
    case tree: RefTree.Ref ⇒ tree.copy(elide = elide)
  }
}

object RefTree {
  /** A special case [[RefTree]] for `null` values. */
  case class Null(highlight: Boolean = false, elide: Boolean = false) extends RefTree {
    def id = "null"
  }

  /** A [[RefTree]] for a value */
  case class Val(
    value: AnyVal,
    hint: Option[Val.Hint],
    highlight: Boolean,
    elide: Boolean
  ) extends RefTree {
    def id = value.toString
  }

  object Val {
    /** Special visualiation hints */
    sealed trait Hint
    case object Bin extends Hint

    /** Construct a [[RefTree]] for a value */
    def apply(value: AnyVal): Val = Val(value, None, highlight = false, elide = false)
  }

  /**
   * A [[RefTree]] for an object
   *
   * Use the companion `apply` method to construct this tree.
   */
  case class Ref private (
    name: String,
    id: String,
    children: Seq[RefTree],
    highlight: Boolean,
    elide: Boolean
  ) extends RefTree {
    def rename(name: String) = copy(name = name)
  }

  object Ref {
    /**
     * Construct a [[RefTree]] for an object
     *
     * The identifier of the tree will be derived automatically.
     * Only modify it (e.g. via `copy`) if you know what you are doing ;)
     */
    def apply(value: AnyRef, children: Seq[RefTree]): Ref = Ref(
      // getSimpleName sometimes does not work, see https://issues.scala-lang.org/browse/SI-5425
      try { value.getClass.getSimpleName }
      catch { case _: InternalError ⇒ value.getClass.getName.replaceAll("^.+\\$", "") },
      s"${value.getClass.getName}${System.identityHashCode(value)}",
      children,
      highlight = false,
      elide = false
    )
  }
}

/**
 * A typeclass for mapping data to [[RefTree]] representations
 */
@implicitNotFound("To render a diagram for type ${A}, implement an instance of reftree.core.ToRefTree[${A}]")
trait ToRefTree[A] { self ⇒
  def refTree(value: A): RefTree

  /** Obtain a new mapping where the field at `index` is highlighted */
  def highlightField(index: Int) = ToRefTree[A] { value ⇒
    self.refTree(value).modify(_.when[RefTree.Ref].children.at(index)).using(_.withHighlight(true))
  }

  /** Obtain a new mapping where the field at `index` is elided */
  def elideField(index: Int) = ToRefTree[A] { value ⇒
    self.refTree(value).modify(_.when[RefTree.Ref].children.at(index)).using(_.withElide(true))
  }

  /** Obtain a new mapping where the field at `index` is elided if the condition is met */
  def elideFieldIf(index: Int, predicate: A ⇒ Boolean) = ToRefTree[A] { value ⇒
    if (!predicate(value)) self.refTree(value) else {
      self.refTree(value).modify(_.when[RefTree.Ref].children.at(index)).using(_.withElide(true))
    }
  }
}

object ToRefTree extends CollectionInstances with GenericInstances {
  /** A shorthand method for creating [[ToRefTree]] instances */
  def apply[A](toRefTree: A ⇒ RefTree): ToRefTree[A] = new ToRefTree[A] {
    def refTree(value: A) = toRefTree(value)
  }

  implicit def `AnyVal RefTree`[A <: AnyVal]: ToRefTree[A] = ToRefTree[A](RefTree.Val.apply)

  implicit def `String RefTree`: ToRefTree[String] = ToRefTree[String] { value ⇒
    RefTree.Ref(value, value.map(RefTree.Val.apply))
  }
}
