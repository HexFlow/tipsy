package tipsy.compare

import Types._
import io.circe.syntax._
import tipsy.frontend.JsonSupport
// import tipsy.parser._

object RealCorrection extends JsonSupport {

  // private def findLimitOfToken(token: CFEnum, line: String, col: String): (Int, Int) = {
  //   token match {
  //     case IFCOND() => col + 1
  //     case LOOPCOND(x) => col + x.length - 1
  //     case RETURN() => col + 5
  //     case BLOCKOPEN() => col
  //     case BLOCKCLOSE() => col
  //   }
  // }

  sealed trait LocationOfChange
  case class Region(start: (Int, Int), end: (Int, Int)) extends LocationOfChange
  case class Point(at: (Int, Int)) extends LocationOfChange

  case class Correction(
    code1: Vector[String],
    code2: Vector[String],
    diff: Diff
  ) {
    val location = diff match {
      case AddDiff(_, prev, _) => {
      }
      case _ =>
    }

    override def toString() = {
      val (l, c) = diff.lineAndCol
      "At line:\n" +
       code1(l-1).drop(c-1) + "\n" +
      "The following diff:\n" + diff.asJson
    }
  }

  def apply(code1: Vector[String], code2: Vector[String], dist: Double, diffs: Vector[Diff]) = {
    diffs.map(Correction(code1, code2, _))
  }
}
