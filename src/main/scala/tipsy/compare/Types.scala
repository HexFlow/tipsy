package tipsy.compare

import tipsy.parser._

/** Contains types of intermediate and result types for the
  * program comparision methods.
  */
object Types {

  sealed trait DiffChange {
    def string(): String = {
      this match {
        case ADD_d => "Add"
        case DEL_d => "Delete"
        case REPLACE_d => "Replace"
      }
    }
  }
  case object ADD_d     extends DiffChange
  case object DEL_d     extends DiffChange
  case object REPLACE_d extends DiffChange

  trait Diff {
    val fxn: String
    lazy val stringWithFxn: String = this.toString ++ " in " ++ fxn
    def setFxn(name: String): Diff
    def position(): String
  }

  case class AddDiff(add: CFEnum, prevPos: String, fxn: String = "") extends Diff {
    override def toString() = {
      "Add    : " ++ add.toString
    }
    def setFxn(name: String): Diff = this.copy(fxn = name)
    def position() = prevPos
  }
  case class DelDiff(del: CFEnum, fxn: String = "") extends Diff {
    override def toString() = {
      "Del    : " ++ del.toString
    }
    def setFxn(name: String): Diff = this.copy(fxn = name)
    def position() = del.position
  }
  case class ReplaceDiff(add: CFEnum, del: CFEnum, fxn: String = "") extends Diff {
    override def toString() = {
      "Replace: " ++ del.toString ++ " with " ++ add.toString
    }
    def setFxn(name: String): Diff = this.copy(fxn = name)
    def position() = del.position
  }

  case class EditRet (diffs: List[Diff], dist: Double) {
    def correct(d: Diff, dis: Double): EditRet = {
      this.copy(diffs = diffs ++ List(d), dist = dist + dis)
    }

    def +(v: Double): EditRet = {
      EditRet(diffs, dist + v)
    }
    def *(v: Double): EditRet = {
      EditRet(diffs, dist * v)
    }

    def +(v: EditRet): EditRet = {
      EditRet(diffs ++ v.diffs, dist + v.dist)
    }

    def /(v: Double) = {
      EditRet(diffs, dist/v)
    }
    def >(x: EditRet): Boolean = {
      dist > x.dist
    }
    def <=(x: EditRet): Boolean = {
      dist <= x.dist
    }
    def min(x: EditRet): EditRet = {
      if (this <= x) this else x
    }
    def apply(a: EditRet) {
      this.copy(diffs = a.diffs, dist = a.dist)
    }
  }
}
