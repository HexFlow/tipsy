package tipsy.compare

import scala.concurrent.Future
import scala.sys.process._


import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._
import tipsy.frontend._

import io.circe.parser._

trait ClusterActions extends TipsyDriver {

  var cnt = 0

  def mean(l: List[Double]) = {
    l.sum / l.length
  }
  def sqr(x: Double) = {
    x*x
  }
  def getVariance(x: List[Double]) = {
    scala.math.sqrt(mean(x.map(sqr)) - sqr(mean(x)))
  }

  def findVarianceOfQues(quesId: String): Future[Double] = {
    for {
      progScores <-driver.runDB {
        progTable.filter(_.quesId === quesId).map(_.score).result
      }.map(_.map(_.toDouble))
    } yield getVariance(progScores.toList)
  }

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
