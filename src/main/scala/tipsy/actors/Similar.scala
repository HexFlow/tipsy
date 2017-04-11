package tipsy.actors

import akka.actor.Actor
import akka.event.Logging

import tipsy.db._
import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._

import tipsy.frontend._
import tipsy.compare._

import spray.json._

class SimilarActor extends Actor with Ops with JsonSupport with TipsyDriver {
  import Messages._

  val log = Logging(context.system, this)

  def receive = {
    case SimilarCheck(id) =>

      val prog: Option[Program] = driver.runDB {
        progTable.filter(_.id === id).result
      }.headOption

      log.debug("Found program")

      prog match {
        case None => sender ! "Not found"
        case Some(p) => {
          val props = p.props.convertTo[Stats]
          val similarProgs: List[Program] = driver.runDB {
            progTable.filter { row =>
              row.quesId === p.quesId
              // row.correct === true &&
              // row.userId =!= p.userId
            }.result
          }.toList

          val similar = similarProgs.filter { elem =>
            val stats = elem.props.convertTo[Stats]

            math.abs(stats.fxns - props.fxns) <= 1 &&
            math.abs(stats.ifs - props.ifs) <= 1 &&
            math.abs(stats.loops - props.loops) <= 1
          }

          sender ! SimilarCheckResp(similar)
        }
      }
  }
}
