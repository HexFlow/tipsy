package tipsy.actors

import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging

import scala.concurrent.{ Future, Await }
import scala.concurrent.duration.Duration

import java.io.File
import java.io.PrintWriter

import tipsy.frontend._
import tipsy.compare._
import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._

trait TipsyActor extends Actor {
  def runInf[T](f: => Future[T]): T = {
    Await.result(f, Duration.Inf)
  }
}

class UpdateDistsActor extends TipsyActor with TipsyDriver {
  val log = Logging(context.system, this)

  def receive = {
    case (id: Int, quesId: String) =>
      println(s"Adding ${id} to dists table.")
      val action = for {
        newNormCode <- driver.runDB {
          progTable.filter(_.id === id).map(_.cf).result
        }.map(_.headOption.getOrElse(throw new Exception("no such program found")))

        otherProgs <- driver.runDB {
          progTable.filter(e => e.id =!= id && e.quesId === quesId).map(e => (e.id, e.cf))result
        }

        newDists = otherProgs.map {
          case (otherId, normCode) => (otherId -> NewLeastEdit.findDist(newNormCode, normCode).dist)
        }.toMap

        d = Dist (
          id = id,
          quesId = quesId,
          dists = newDists
        )

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

        matrix <- driver.runDB {
          distTable.filter(_.quesId === quesId).map(e => (e.id, e.dists)).result
        }
      } yield matrix

      val matrix = runInf(action)

      val matrixStr = matrix.map {
        case (id, distMap) => s"${id}: " ++
          distMap.toList.map {
            case (nid, dist) => s"(${nid}, ${dist})"
          }.mkString(", ")
      }.mkString("\n")

      val writer = new PrintWriter(new File(s"matrix_${quesId}"))
      writer.write(matrixStr)
      writer.close()
  }
}
