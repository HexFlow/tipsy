package tipsy.db

import tipsy.db.schema._
import tipsy.compare._
import tipsy.parser._

import io.circe._, io.circe.generic.semiauto._, io.circe.generic.JsonCodec, io.circe.syntax._

/**
  * Includes case classes for expected data bodies in web requests
  */
object Requests {
  @JsonCodec case class ProgramInsertReq (
    id: Option[Int],
    userId: String,
    quesId: String,
    code: String,
    updateClusters: Option[Boolean]
  )
}
