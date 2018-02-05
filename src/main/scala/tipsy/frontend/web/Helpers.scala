package tipsy.frontend.web

import tipsy.frontend.Requests
import tipsy.compiler._
import tipsy.compare._
import tipsy.parser._
import tipsy.db._
import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._
import tipsy.frontend._

import akka.http.scaladsl.model.StatusCode
import scala.concurrent.Future

import io.circe.Json

trait Helpers extends Ops with TipsyDriver {
  type HandleResp = Future[(StatusCode, Json)]

  // Inserts provided program into database, or updates existing program.
  def insertProg(prog: Program) = {
    // Operation depends on whether an ID was provided
    for {
      id <- prog.id match {
        case 0 => {
          println("Inserting into a new row")
          insert(prog, progTable)
        }
        case idReq => {
          println("Updating id: " + idReq)
          driver.runDB { progTable.insertOrUpdate(prog) }
          Future(idReq)
        }
      }
      _ <- Future(updateDists ! (id, prog.quesId))
    } yield id // Return the ID to sender parent
  }

  def getFromDB(id: Int): Future[Option[Program]] = {
    driver.runDB {
      progTable.filter(_.id === id).result
    }.map(_.headOption)
  }
}
