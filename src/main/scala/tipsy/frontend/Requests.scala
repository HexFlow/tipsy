package tipsy.frontend

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.DefaultJsonProtocol
import spray.json._

import tipsy.db.schema._
import tipsy.compare._
import tipsy.parser._

/**
  * Collect your json format instances into a support trait
  */
trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  import Requests._
  implicit val progReqFormat = jsonFormat4(ProgramInsertReq)
  implicit val progRespFormat = jsonFormat8(Program)
  implicit val statsFormat = jsonFormat4(Stats)

  object CFEnumFormat extends RootJsonFormat[CFEnum] {
    def write(a: CFEnum) = a match {
      case POSTEXPR(e) => ("Expr: " + e mkString "").toJson
      case EXPR(e) => (a.flowName + ": " + e.toString).toJson
      case _ => a.flowName.toJson
    }
    def read(va: JsValue) = ???
  }

  object DiffFormat extends RootJsonFormat[Diff] {
    def write(a: Diff) = a match {
      case Diff(ADD_d, Some(x), None) => JsObject (
        "change" -> "Add+".toJson,
        "addEntry" -> x.toJson
        )
      case Diff(DEL_d, None, Some(x)) => JsObject (
        "change" -> "Remove-".toJson,
        "removeEntry" -> x.toJson
        )
      case Diff(REPLACE_d, Some(x), Some(y)) => JsObject (
        "change" -> "Replace+-".toJson,
        "addEntry" -> x.toJson,
        "removeEntry" -> y.toJson
        )
    }
    def read(va: JsValue) = ???
  }

  implicit val cfenumFormat = lazyFormat(CFEnumFormat)
  implicit val diffFormat = lazyFormat(DiffFormat)
  implicit val editRetFormat = lazyFormat(jsonFormat2(EditRet))
}

/**
  * Includes case classes for expected data bodies in web requests
  */
object Requests {
  case class ProgramInsertReq (
    id: Option[Int],
    userId: String,
    quesId: String,
    code: String
  )
}
