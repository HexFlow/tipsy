package tipsy.actors

import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging

import tipsy.compiler._
import tipsy.compare.ProgStats
import tipsy.db.schema._
import tipsy.frontend._
import tipsy.frontend.Requests._

import spray.json._

import java.io._
import scala.sys.process._
import scala.util.Random

class CompileActor extends Actor with JsonSupport {
  val log = Logging(context.system, this)

  def receive = {
    case prog: ProgramInsertReq => {

      val idReq: Int = prog.id.getOrElse(0)

      val curtime = System.currentTimeMillis().toString()
      val filename =
        "." + curtime + "-" + Random.alphanumeric.take(5).mkString + ".c"

      val w = new BufferedWriter(new FileWriter(filename))
      w.write(prog.code)
      w.close()

      WorkflowCompiler(filename) match {
        case Right(tree) => {
          log.debug("Compiled code")
          sender ! Program(
            id      = idReq,
            userId  = prog.userId,
            time    = curtime,
            quesId  = prog.quesId,
            code    = prog.code,
            score   = "0",
            correct = false,
            props   = ProgStats(tree).toJson // TODO Store statistics here
          )
        }

        case Left(err) => {
          log.error("Compilation error =>")
          log.error(err.toString)
          sender ! err.toString
        }
      }

      s"rm ${filename}".!
    }
  }
}
