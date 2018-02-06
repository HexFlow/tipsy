package tipsy.db


import io.circe.generic.JsonCodec

/**
  * Includes case classes for expected data bodies in web requests
  */
object Requests {
  @JsonCodec case class ProgramInsertReq (
    id: Option[Int],
    userId: String,
    quesId: String,
    code: String,
    score: String,
    updateClusters: Option[Boolean]
  )
}
