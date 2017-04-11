package tipsy.frontend

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.DefaultJsonProtocol
import spray.json._

import tipsy.db.schema._

/**
  * Collect your json format instances into a support trait
  */
trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  import Requests._
  implicit val progReqFormat = jsonFormat4(ProgramInsertReq)
  implicit val progRespFormat = jsonFormat8(Program)
  implicit val statsFormat = jsonFormat4(Stats)
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
