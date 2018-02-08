package tipsy.compare

import scala.concurrent.{ Future, Await }
import scala.concurrent.duration.Duration
import scala.sys.process._

import java.io.File
import java.io.PrintWriter

import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._
import tipsy.frontend._

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

trait ClusterActions extends TipsyDriver {

  var cnt = 0

  def doUpdateClusters(matrix: Seq[(Int, Int, Double)], quesId: String): Future[Unit] = {
    cnt = cnt + 1

    val matrixStr = Dists.getAsJson(matrix)

    // Ugly way to get output of clustering.
    val cmd = List("bash", "-c", s"python2 scripts/hierarchical_clustering.py 2> errors/clust_errlog-${cnt.toString}")
    val is = new java.io.ByteArrayInputStream(matrixStr.getBytes("UTF-8"))
    val out = (cmd #< is).lines_!
    val res = out.mkString("")

    decode[List[List[Int]]](res) match {
      case Left(err) =>
        println("Error in parsing clustering response.")
        println("Error was: " ++ err.toString)
        println("Output was: " ++ res)
        Future()
      case Right(clusterList) =>
        for {
          _ <- driver.runDB {
            clusterTable.insertOrUpdate(Cluster(
              quesId = quesId,
              cluster = clusterList
            ))
          }
          _ <- Future(println(s"Finished updating clusters."))
        } yield ()
    }
  }
}
