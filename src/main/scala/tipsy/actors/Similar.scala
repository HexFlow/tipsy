package tipsy.actors

import akka.actor.Actor
import akka.event.Logging

import tipsy.db._
import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._

import tipsy.frontend._
import tipsy.compare._

class SimilarActor extends Actor with Ops with JsonSupport with TipsyDriver {
  val log = Logging(context.system, this)

  def receive = {
    case id: Int =>
      val prog: Option[Program] = driver.runDB {
        progTable.filter(_.id === id).result
      }.headOption

      prog match {
        case None => sender ! "Not found"
        case Some(p) => {
          val props = p.props.convertTo[Stats]
          val similarProgs = driver.runDB {
            progTable.filter {
              (_.props.+>>("fxns") <= (props.fxns + 1).toString) &&
              (_.props.+>>("fxns") >= (props.fxns - 1).toString)
            }.map(_.code).result
          }

          sender ! Corrections(2)
        }
      }
  }
}
