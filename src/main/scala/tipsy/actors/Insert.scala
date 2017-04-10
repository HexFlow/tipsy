package tipsy.actors

import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging

import tipsy.compiler._
import tipsy.db._
import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._
import tipsy.frontend._
import tipsy.frontend.Requests._

import spray.json._

import java.io._
import scala.sys.process._

class InsertActor extends Actor with Ops with JsonSupport with TipsyDriver {

  val log = Logging(context.system, this)

  def receive = {
    case progWithId: (Int, ProgramInsertReq) => {

      val prog = progWithId._2
      val idReq = progWithId._1

      val curtime = System.currentTimeMillis().toString()
      val filename = curtime + ".c"

      val w = new BufferedWriter(new FileWriter(filename))
      w.write(prog.code)
      w.close()

      WorkflowCompiler(filename) match {
        case Right(tree) => {

          val tupProg = Program(
            idReq,
            prog.userId,
            curtime,
            prog.quesId,
            prog.code,
            "0",
            false,
            "{}".toJson // TODO Store statistics here
          )

          // Operation depends on whether an ID was provided
          val id: Int =
            if (idReq == 0) insert(tupProg, progTable)
            else {
              driver.runDB { progTable.insertOrUpdate(tupProg) }
              idReq
            }

          // Return the ID to sender parent
          sender ! id
        }

        case Left(err) => {
          println("Compilation error =>")
          println(err)
          sender ! err.toString
        }
      }

      s"rm ${filename}".!
    }
  }
}
