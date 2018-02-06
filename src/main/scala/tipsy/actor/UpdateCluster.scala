package tipsy.actors


import scala.concurrent.{ Future, Await }
import scala.concurrent.duration.Duration
import scala.sys.process._

import java.io.File
import java.io.PrintWriter

import tipsy.frontend._
import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._

class UpdateClustersActor extends TipsyActor with TipsyDriver with TipsyActors {
  implicit override val executionContext = system.dispatchers.lookup("my-pinned-dispatcher")

  def receive = {
    case (quesId: String) =>
      val action = for {
        matrix <- driver.runDB {
          distTable.filter(_.quesId === quesId).map(e => (e.id, e.dists)).result
        }

        matrixStr = Dists.getAsDump(matrix)

        writer = new PrintWriter(new File(s"matrix_${quesId}"))
        _ <- Future(writer.write(matrixStr))
        _ <- Future(writer.close())

        // Ugly way to get output of clustering.
        cmd = List("bash", "-c", "python2 scripts/hierarchical_clustering.py 2> errlog")
        is = new java.io.ByteArrayInputStream(matrixStr.getBytes("UTF-8"))
        out = (cmd #< is).lines_!
        res = out.mkString("")
        clusterList = res.split('|').map(_.split(',').map(_.toInt).toList).toList
        _ <- Future(println(clusterList))

        _ <- driver.runDB {
          clusterTable.insertOrUpdate(Cluster(
            quesId = quesId,
            cluster = clusterList
          ))
        }

        _ <- Future(println(s"Finished updating clusters."))
      } yield ()
      Await.result(action, Duration.Inf)
  }
}
