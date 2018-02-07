package tipsy.actors

import akka.actor.Actor

import scala.concurrent.{ Future, Await }
import scala.concurrent.duration.Duration
import scala.collection.parallel.immutable.ParVector

import java.io.File
import java.io.PrintWriter

import akka.actor._

import tipsy.frontend._
import tipsy.compare._
import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._

class UpdateDistsParActor extends TipsyActor with TipsyDriver with TipsyActors {
  implicit override val executionContext = system.dispatchers.lookup("tipsy-blocking-dispatcher")

  val updateClustersActor = system.actorOf(Props(classOf[UpdateClustersActor]), "updateClustersActor")

  def receive = {
    case UpdateReq(id, quesId, newNormCode, otherProgs, shouldUpdateClusters) =>
      val st = System.currentTimeMillis()

      val newDists = otherProgs.par.map {
        case (otherId, normCode) =>
          (otherId -> Compare.findDist(newNormCode, normCode).dist)
      }.toList.toMap

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

        _ <- Future(scala.tools.nsc.io.Path(s"dist_time_${quesId}").createFile()
          .appendAll((System.currentTimeMillis() - st).toString ++ "\n"))

        _ <- if (shouldUpdateClusters) {
          for {
            matrix <- driver.runDB {
              distTable.filter(_.quesId === quesId).map(e => (e.id, e.dists)).result
            }
          } yield updateClustersActor ! UpdateClusterMsg(quesId, matrix)
        } else Future()
      } yield ()

      Await.result(action, Duration.Inf)


    case _ => println("Unknown type of message received in updateDists actor.")
  }
}
