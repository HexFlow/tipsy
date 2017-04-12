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
            try {
              val stats = elem.props.convertTo[Stats]
              val sat = for {
                f1 <- stats.fxns
                f2 <- props.fxns
                r1 <- Some(math.abs(f1 - f2))
                if (r1 <= 1)

                i1 <- stats.ifs
                i2 <- props.ifs
                r2 <- Some(math.abs(i1 - i2))
                if (r2 <= 1)

                l1 <- stats.loops
                l2 <- stats.loops
                r3 <- Some(math.abs(l1 - l2))
                if (r3 <= 1)

              } yield ()

              sat match {
                case Some(_) => true
                case _ => false
              }
            } catch {
              case e: Throwable => false
            }
          }

          sender ! SimilarCheckResp(similar)
        }
      }
  }
}
