package tipsy.compare

import tipsy.parser._


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

case class Diff (
  change: DiffChange,
  addEntry: Option[CFEnum],
  delEntry: Option[CFEnum],
  fxn: String = "") {
  lazy val position = delEntry.map(x => (x.line, x.column))
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
