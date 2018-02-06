package tipsy.actors

import scala.concurrent.{ Future, Await }
import scala.concurrent.duration.Duration

import tipsy.frontend._
import tipsy.compare.ClusterActions

class UpdateClustersActor extends TipsyActor with TipsyDriver with TipsyActors
    with ClusterActions {

  implicit override val executionContext = system.dispatchers.lookup("my-pinned-dispatcher")

  def receive = {
    case (quesId: String) =>
      Await.result(doUpdateClusters(quesId), Duration.Inf)
  }
}
