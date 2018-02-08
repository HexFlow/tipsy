package tipsy.frontend.web

import tipsy.db._
import tipsy.actors._
import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._
import tipsy.frontend._
import tipsy.compare.NormCode

import akka.http.scaladsl.model.StatusCode
import scala.concurrent.Future

import io.circe.Json

trait Helpers extends Ops with TipsyDriver with TipsyActors {
  type HandleResp = Future[(StatusCode, Json)]

  def getFromDB(id: Int): Future[Option[Program]] = {
    driver.runDB {
      progTable.filter(_.id === id).result
    }.map(_.headOption)
  }
}
