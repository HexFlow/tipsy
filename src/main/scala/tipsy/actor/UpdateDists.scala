package tipsy.actors

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.{ Future, Await }
import scala.concurrent.duration._

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

case class UpdateReq(
  id: Int,
  quesId: String,
  newNormCode: NormCode,
  otherProgs: Vector[(Int, NormCode)],
  shouldUpdateClusters: Boolean
)

class UpdateDistsActor extends TipsyActor with TipsyDriver with TipsyActors {
  implicit override val executionContext = system.dispatchers.lookup("my-pinned-dispatcher")
  implicit val timeout = new Timeout(100 seconds)

  val updateDistsParActor = system.actorOf(Props(classOf[UpdateDistsParActor]), "updateDistsParActor")

  def receive = {
    case x: UpdateReq =>
      val action = for {
        _ <- Future(println(s"Adding ${x.id} to dists table. Time: " ++ System.currentTimeMillis().toString))
        _ <- updateDistsParActor ? x
        _ <- Future(println(s"Finished updating dists after ${x.id}. Time: " ++ System.currentTimeMillis().toString))
      } yield ()
      Await.result(action, Duration.Inf)

    case _ => println("Unknown type of message received in updateDists actor.")
  }
}
