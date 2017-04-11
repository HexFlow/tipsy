package tipsy.actors

import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging

import akka.pattern.ask
import akka.util.Timeout

import tipsy.db._
import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._
import tipsy.frontend._
import tipsy.frontend.Requests._

class InsertActor extends Actor with Ops with JsonSupport with TipsyDriver {
  import Messages._

  val log = Logging(context.system, this)

  def receive = {
    case InsertReq(prog) => {

      // Operation depends on whether an ID was provided
      val id: Int = prog.id match {
        case 0 => {
          log.debug("Inserting into a new row")
          insert(prog, progTable)
        }
        case idReq => {
          log.debug("Updating id: ", idReq)
          driver.runDB { progTable.insertOrUpdate(prog) }
          idReq
        }
      }

      // Return the ID to sender parent
      log.debug("Saved as id: " + id.toString)
      sender ! InsertResp(id)
    }
  }
}
