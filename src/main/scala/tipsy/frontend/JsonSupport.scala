package tipsy.frontend

import tipsy.compare._
import tipsy.parser._
import Types._

import io.circe._
import io.circe.syntax._
import io.circe.{Encoder, Json }

/**
  * Collect your json format instances into a support trait
  */
trait JsonSupport {

  implicit protected def allToJson[T](x: T)(implicit encoder: io.circe.Encoder[T]) = x.asJson

  implicit protected val encodeCF: Encoder[CFEnum] = new Encoder[CFEnum] {
    final def apply(a: CFEnum): Json = a match {
      case POSTEXPR(e) => ("Expr: " + e mkString "").asJson
      case _ => a.flowName.asJson
    }
  }

  implicit protected val encodeDiff: Encoder[Diff] = new Encoder[Diff] {
    final def apply(a: Diff): Json =
      a match {
        case AddDiff(add, _, fxn) => Json.obj (
          "change" -> "Add",
          "addition" -> add.asJson,
          "position" -> a.position,
          "function" -> fxn
        )
        case DelDiff(del, fxn) => Json.obj (
          "change" -> "Delete",
          "deletion" -> del.asJson,
          "position" -> a.position,
          "function" -> fxn
        )
        case ReplaceDiff(add, del, fxn) => Json.obj (
          "change" -> "Replace",
          "addition" -> add.asJson,
          "deletion" -> del.asJson,
          "position" -> a.position,
          "function" -> fxn
        )
      }
  }
}
