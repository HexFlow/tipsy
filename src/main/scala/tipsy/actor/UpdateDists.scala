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

class UpdateDistsActor extends TipsyActor with TipsyDriverWithoutActors {
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

        _ <- Future(println(s"Finished updating dists after ${id}."))

        matrixStr = Dists.getAsDump(matrix)

        // Ugly way to get output of clustering.
        cmd = List("bash", "-c", "nix-shell scripts/shell.nix --run \"python2 scripts/hierarchical_clustering.py\" 2> errlog")
        is = new java.io.ByteArrayInputStream(matrixStr.getBytes("UTF-8"))
        out = (cmd #< is).lines_!
        res = out.mkString("")

        // Ugly way to parse output of clustering.
        k: Array[Array[Int]] = (res.split('|').map(_.trim).map(_.drop(1).dropRight(1).split(',').map(_.trim.toInt)))
        j: List[(Int, Int)] = k.map(x => (x(0), x(1))).toList

        clusterMap: Map[Int, List[Int]] = j.groupBy(_._2).map { case (k, v) => k -> v.map(_._1) }
        clusterList: List[List[Int]] = clusterMap.map { case (_, v) => v }.toList

        _ <- driver.runDB {
          clusterTable.insertOrUpdate(Cluster(
            quesId = quesId,
            cluster = clusterList
          ))
        }

        _ <- Future(println(s"Finished updating clusters after ${id}."))

      } yield ()

      Await.result(action, Duration.Inf)

    case _ => println("Unknown type of message received in updateDists actor.")
  }
}
