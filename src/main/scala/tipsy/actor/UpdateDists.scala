package tipsy.actors

import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging

import scala.concurrent.{ Future, Await }
import scala.concurrent.duration.Duration
import scala.sys.process._

import java.io.File
import java.io.PrintWriter
import java.nio.charset.Charset

import tipsy.frontend._
import tipsy.compare._
import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._

trait TipsyActor extends Actor {
  def runInf[T](f: => Future[T]): T = {
    Await.result(f, Duration.Inf)
  }
}

case class UpdateReq(
  id: Int,
  quesId: String,
  newNormCode: NormCode,
  otherProgs: Seq[(Int, NormCode)],
  shouldUpdateClusters: Boolean
)

class UpdateDistsActor extends TipsyActor with TipsyDriverWithoutActors {
  def receive = {
    case UpdateReq(id, quesId, newNormCode, otherProgs, shouldUpdateClusters) =>
      println(s"Adding ${id} to dists table.")

      val newDists = otherProgs.map {
        case (otherId, normCode) =>
          (otherId -> Compare.findDist(newNormCode, normCode).dist)
      }.toMap

      val d = Dist (
        id = id,
        quesId = quesId,
        dists = newDists
      )

      val action = for {
        _ <- driver.runDB {
          distTable.insertOrUpdate(d)
        }

        _ <- Future.sequence {
          newDists.map {
            case (otherId, distFromNew) => for {
              otherDists <- driver.runDB {
                distTable.filter(_.id === otherId).result
              }.map(_.headOption.getOrElse(throw new Exception(s"program ${otherId} did not have distance entry")))

              updatedOtherDists = otherDists.copy(
                dists = otherDists.dists + (id -> distFromNew))

              _ <- driver.runDB {
                distTable.insertOrUpdate(updatedOtherDists)
              }
            } yield ()
          }
        }

        msg = if (shouldUpdateClusters) {
          updateClusters ! quesId
        } else ()

        _ <- Future(msg)

        _ <- Future(println(s"Finished updating dists after ${id}."))
      } yield ()

      Await.result(action, Duration.Inf)

    case _ => println("Unknown type of message received in updateDists actor.")
  }
}
