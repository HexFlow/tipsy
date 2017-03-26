package tipsy.frontend

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.DefaultJsonProtocol
import spray.json._

/**
  * Collect your json format instances into a support trait
  */
trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  import Requests._
  implicit val progFormat = jsonFormat3(ProgramInsertReq)
}

/**
  * Includes case classes for expected data bodies in web requests
  */
object Requests {
  case class ProgramInsertReq (
    userId: String,
    quesId: String,
    code: String
  )
}
