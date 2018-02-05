package tipsy.frontend

import tipsy.db.schema._
import tipsy.compare._
import tipsy.parser._

import io.circe._, io.circe.generic.semiauto._, io.circe.generic.JsonCodec, io.circe.syntax._
import io.circe.{ Decoder, Encoder, HCursor, Json }

/**
  * Collect your json format instances into a support trait
  */
trait JsonSupport {
  import Requests._

  implicit val encodeCF: Encoder[CFEnum] = new Encoder[CFEnum] {
    final def apply(a: CFEnum): Json = a match {
      case POSTEXPR(e) => ("Expr: " + e mkString "").asJson
      // case EXPR(e) => (a.flowName + ": " + e.toString).asJson
      case _ => a.flowName.asJson
    }
  }

  implicit val encodeDiff: Encoder[Diff] = new Encoder[Diff] {
    final def apply(a: Diff): Json =
      // ("foo", Json.fromString(a.foo)),
      // ("bar", Json.fromInt(a.bar))
      a match {
        case Diff(ADD_d, Some(x), None, f) => Json.obj (
          "change" -> "Add+".asJson,
          "addEntry" -> x.asJson,
          "position" -> a.position.toString.asJson,
          "function" -> f.asJson
        )
        case Diff(DEL_d, None, Some(x), f) => Json.obj (
          "change" -> "Remove-".asJson,
          "removeEntry" -> x.asJson,
          "position" -> a.position.toString.asJson,
          "function" -> f.asJson
        )
        case Diff(REPLACE_d, Some(x), Some(y), f) => Json.obj (
          "change" -> "Replace+-".asJson,
          "addEntry" -> x.asJson,
          "removeEntry" -> y.asJson,
          "position" -> a.position.toString.asJson,
          "function" -> f.asJson
        )
      }
  }

  // implicit val progReqFormat = jsonFormat4(ProgramInsertReq)
  // implicit val progRespFormat = jsonFormat9(Program)

  // implicit val cfenumFormat = lazyFormat(CFEnumFormat)
  // implicit val diffFormat = lazyFormat(DiffFormat)
  // implicit val editRetFormat = lazyFormat(jsonFormat2(EditRet))
}

/**
  * Includes case classes for expected data bodies in web requests
  */
object Requests {
  @JsonCodec case class ProgramInsertReq (
    id: Option[Int],
    userId: String,
    quesId: String,
    code: String
  )
}
