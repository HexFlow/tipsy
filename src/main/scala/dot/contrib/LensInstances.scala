package dot.contrib

import monocle.{Traversal, Lens}
import dot.core._

import scala.annotation.implicitNotFound

/**
 * [[ToRefTree]] instances for [[LensInstances.LensFocus]] that show optics together with their focuses
 */
object LensInstances {
  /** A class to represent a [[Traversal]] together with its target */
  case class LensFocus[A, B](lens: Traversal[A, B], target: A)

  object LensFocus {
    /** Construct a [[LensFocus]] from a lens and its target */
    def apply[A, B](lens: Lens[A, B], target: A): LensFocus[A, B] = LensFocus(lens.asTraversal, target)
  }

  /** A typeclass for generating values of a certain type */
  @implicitNotFound("Could not find a way to generate a value of type ${A}")
  trait Example[A] {
    def exemplify: A
  }

  object Example {
    def apply[A](f: A): Example[A] = new Example[A] {
      def exemplify = f
    }
    implicit val `Int Example`: Example[Int] = Example[Int](42)
    implicit val `Long Example`: Example[Long] = Example[Long](42)
    implicit val `Char Example`: Example[Char] = Example[Char]('?')
    implicit val `String Example`: Example[String] = Example[String]("specimen")

    import shapeless._

    implicit val `HNil Example`: Example[HNil] = Example[HNil](HNil)

    implicit def `HCons Example`[H: Example, T <: HList: Example]: Example[H :: T] = Example[H :: T] {
      implicitly[Example[H]].exemplify :: implicitly[Example[T]].exemplify
    }

    implicit def `Generic Example`[A, L <: HList](
      implicit generic: Generic.Aux[A, L], hListExample: Lazy[Example[L]]
    ): Example[A] = Example[A] {
      generic.from(hListExample.value.exemplify)
    }
  }

  /** A typeclass for marking values of a certain type */
  @implicitNotFound("Could not find a way to mark a value of type ${A}")
  trait Marker[A] {
    def mark(value: A): A
  }

  object Marker {
    def apply[A](f: A ⇒ A): Marker[A] = new Marker[A] {
      def mark(value: A) = f(value)
    }
    implicit val `Int Marker` = Marker[Int](x ⇒ x + 1)
    implicit val `Long Marker` = Marker[Long](x ⇒ x + 1L)
    implicit val `Char Marker` = Marker[Char] { case '?' ⇒ '!'; case _ ⇒ '?' }
    implicit val `String Marker` = Marker[String](x ⇒ x + " ")

    import shapeless._

    implicit def `HCons Marker`[H: Marker, T <: HList]: Marker[H :: T] = Marker[H :: T] {
      case h :: t ⇒ implicitly[Marker[H]].mark(h) :: t
    }

    implicit def `Generic Marker`[A, L <: HList](
      implicit generic: Generic.Aux[A, L], hListMarker: Lazy[Marker[L]]
    ): Marker[A] = Marker[A] { value ⇒
      generic.from(hListMarker.value.mark(generic.to(value)))
    }
  }

  implicit def `LensFocus RefTree`[A, B](
    implicit refTreeA: ToRefTree[A],
    exampleB: Example[B], refTreeB: ToRefTree[B],
    marker: Marker[B]
  ): ToRefTree[LensFocus[A, B]] = ToRefTree[LensFocus[A, B]] { value ⇒
    // modify the target using the lens and the marker function
    val modified = value.lens.modify(marker.mark)(value.target)
    // an example RefTree for type B used to detect similar RefTrees
    val example = exampleB.exemplify.refTree

    def inner(tree1: RefTree, tree2: RefTree): RefTree = (example, tree1, tree2) match {
      case (_: RefTree.Val, x: RefTree.Val, y: RefTree.Val) if x != y ⇒
        // the example is a Val, and we found two mismatching Val trees
        x.copy(highlight = true)

      case (RefTree.Ref(name, _, _, _, _), x: RefTree.Ref, y: RefTree.Ref) if x != y && x.name == name ⇒
        // the example is a Ref, and we found two mismatching Ref trees with the same name
        x.copy(highlight = true)

      case (_, x: RefTree.Ref, y: RefTree.Ref) ⇒
        // recurse
        val children = (x.children zip y.children) map { case (cx, cy) ⇒ inner(cx, cy) }
        x.copy(children = children)

      case _ ⇒ tree1
    }

    // compare the RefTrees before and after modification
    inner(value.target.refTree, modified.refTree)
  }
}
