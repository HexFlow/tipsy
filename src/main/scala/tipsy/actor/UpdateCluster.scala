package tipsy.actors

import scala.concurrent.{ Future, Await }
import scala.concurrent.duration.Duration

import java.io.File
import java.io.PrintWriter

import tipsy.frontend._
import tipsy.compare.ClusterActions

case class UpdateClusterMsg(
  quesId: String,
  matrix: Seq[(Int, Map[Int, Double])]
)

class UpdateClustersActor extends TipsyActor with TipsyDriver with TipsyActors
    with ClusterActions {

  implicit override val executionContext = system.dispatchers.lookup("my-pinned-dispatcher")

  val writer = new PrintWriter(new File(s"clust_times"))

  def receive = {
    case x: UpdateClusterMsg =>
      println(s"Updating clusters. Time: " ++ System.currentTimeMillis().toString)
      val st = System.currentTimeMillis()
      Await.result(doUpdateClusters(x.matrix, x.quesId), Duration.Inf)
      println(s"Finished updating clusters. Time: " ++ System.currentTimeMillis().toString)
      writer.write((System.currentTimeMillis() - st).toString ++ "\n")
      writer.flush()

    case _ => println("Unknown type of message received in updateClusters actor.")
  }
}
