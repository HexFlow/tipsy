package tipsy.actors

import scala.concurrent.{ Future, Await }
import scala.concurrent.duration.Duration

import java.io.File
import java.io.PrintWriter

import tipsy.frontend._
import tipsy.compare.ClusterActions

case class UpdateClusterMsg(
  quesId: String,
  matrix: Seq[(Int, Int, Double)]
)

class UpdateClustersActor extends TipsyActor with TipsyDriver with TipsyActors
    with ClusterActions {

  implicit override val executionContext = system.dispatchers.lookup("my-pinned-dispatcher")

  def receive = {
    case x: UpdateClusterMsg =>
      println(s"Updating clusters. Time: " ++ System.currentTimeMillis().toString)
      val st = System.currentTimeMillis()
      Await.result(doUpdateClusters(x.matrix, x.quesId), Duration.Inf)
      println(s"Finished updating clusters. Time: " ++ System.currentTimeMillis().toString)
      scala.tools.nsc.io.Path(s"clust_time_${x.quesId}").createFile()
        .appendAll((System.currentTimeMillis() - st).toString ++ "\n")

    case _ => println("Unknown type of message received in updateClusters actor.")
  }
}
