package tipsy.compare

import scala.concurrent.{ Future, Await }
import scala.concurrent.duration.Duration
import scala.sys.process._

import java.io.File
import java.io.PrintWriter

import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._

import tipsy.frontend._

trait ClusterActions extends TipsyDriver {

  var cnt = 0

  def doUpdateClusters(matrix: Seq[(Int, Map[Int, Double])], quesId: String) = {
    cnt = cnt + 1

    val matrixStr = Dists.getAsDump(matrix)

    val writer = new PrintWriter(new File(s"matrix_${quesId}"))
    writer.write(matrixStr)
    writer.close()

    val writer2 = new PrintWriter(new File(s"errors/matrix_${cnt.toString}"))
    writer2.write(matrixStr)
    writer2.close()

    // Ugly way to get output of clustering.
    val cmd = List("bash", "-c", s"python2 scripts/hierarchical_clustering.py 2> errors/clust_errlog-${cnt.toString}")
    val is = new java.io.ByteArrayInputStream(matrixStr.getBytes("UTF-8"))
    val out = (cmd #< is).lines_!
    val res = out.mkString("")
    println("Output is: " ++ res)
    val clusterList = res.split('|').map(_.split(',').map(_.toInt).toList).toList
    println(clusterList)

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
