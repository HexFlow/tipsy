package tipsy.actors

import akka.actor.Actor

import scala.concurrent.{ Future, Await }
import scala.concurrent.duration.Duration
import scala.collection.parallel.immutable.ParVector
import scala.util.{ Try, Success, Failure }

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

  def parallely[T](dbios: List[DBIO[T]]): Future[List[Try[T]]] = {
    Future.traverse(dbios.map(driver.runDB(_))) { f =>
      f.map(Success(_))
        .recover { case th => Failure(th)} }
  }

  def receive = {
    case UpdateReq(id, quesId, newNormCode, otherProgs, shouldUpdateClusters) =>
      val st = System.currentTimeMillis()

      val newDists = otherProgs.par.map {
        case (otherId, normCode) =>
          Dists.createDistEntry(id, otherId, quesId, Compare.findDist(newNormCode, normCode).dist)
      }.toList

      val action = for {
        _ <- parallely {
          newDists.map (distTable.insertOrUpdate(_))
        }
        _ <- Future(scala.tools.nsc.io.Path(s"dist_time_${quesId}").createFile()
          .appendAll((System.currentTimeMillis() - st).toString ++ "\n"))

        _ <- if (shouldUpdateClusters) {
          for {
            matrix <- driver.runDB {
              distTable.filter(_.quesId === quesId).map(e => (e.id1, e.id2, e.dist)).result
            }
          } yield updateClustersActor ! UpdateClusterMsg(quesId, matrix)
        } else Future()
      } yield ()

      Await.result(action, Duration.Inf)
      sender ! ()

    case _ => println("Unknown type of message received in updateDistsPar actor.")
  }
}
