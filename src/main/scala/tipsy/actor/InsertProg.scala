package tipsy.actors

import akka.actor._

import scala.concurrent.{ Future, Await }
import scala.concurrent.duration.Duration
import scala.collection.mutable.{Map => mMap}

import tipsy.frontend._
import tipsy.db.TipsyPostgresProfile.api._
import tipsy.compare.NormCode
import tipsy.db.schema._
import tipsy.db.Ops

case class InsertProgMsg(
  prog: Program,
  updateClusters: Boolean
)

class InsertProgActor extends TipsyActor with TipsyDriver with TipsyActors with Ops {

  implicit override val executionContext = system.dispatchers.lookup("my-pinned-dispatcher")
  val updateDistsActor = system.actorOf(Props(classOf[UpdateDistsActor]), "updateDistsActor")

  val otherProgs: mMap[String, Vector[(Int, NormCode)]] = mMap()

  def initialize() = {
    println("Initializing insertProgActor")
    val action = for {
      fetchOtherProgs <- driver.runDB {
        progTable.map(e => (e.quesId, e.id, e.cf)).result
      }
    } yield fetchOtherProgs
    val fetchOtherProgs = Await.result(action, Duration.Inf)
    otherProgs.clear()
    fetchOtherProgs.map(x => otherProgs.put(x._1,
      otherProgs.getOrElseUpdate(x._1, Vector()) :+ ((x._2, x._3))))
    println(s"OtherProgs now has ${otherProgs.map(_._2.length).sum} elements")
  }

  override def preStart() = {
    initialize()
  }

  override def preRestart(reason: Throwable, message: Option[Any]) = {
    initialize()
  }

  def receive = {
    case InsertProgMsg(prog, updateClusters) =>
      val action = for {
        // Operation depends on whether an ID was provided
        id <- prog.id match {
          case 0 => {
            insert(prog, progTable)
          }
          case idReq => {
            driver.runDB { progTable.insertOrUpdate(prog) }
            Future(idReq)
          }
        }
        _ <- Future(println("Inserting into id: " ++ id.toString))

        newNormCode <- driver.runDB {
          progTable.filter(_.id === id).map(_.cf).result
        }.map(_.headOption.getOrElse(throw new Exception("no such program found")))

        _ <- Future(
          otherProgs.put(prog.quesId,
            otherProgs.getOrElseUpdate(prog.quesId, Vector()) :+ ((id, newNormCode))
          )
        )
      } yield (id, newNormCode) // Return the ID to sender parent

      val (id, newNormCode) = Await.result(action, Duration.Inf)
      updateDistsActor ! UpdateReq(id, prog.quesId, newNormCode, otherProgs(prog.quesId).filter(_._1 != id), updateClusters)
      sender ! id

    case _ => println("Unknown type of message received in insertProg actor.")
  }
}
