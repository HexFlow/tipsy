package tipsy.frontend.web

import scalaz._
import tipsy.db.Requests
import tipsy.compiler._
import tipsy.db._
import tipsy.db.TipsyPostgresProfile.api._
import tipsy.actors.InsertProgMsg
import akka.http.scaladsl.model.StatusCodes._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Future
import scala.util.{Success, Failure}
import scala.concurrent.duration._
import io.circe.syntax._

trait TableHandlers extends Helpers {

  def createSchema(): HandleResp = {
    for {
      _ <- create(progTable)
      _ <- create(clusterTable)
      _ <- create(distTable)
    } yield (OK, "Created schemas".asJson)
  }

  def dropSchema(): HandleResp = {
    for {
      _ <- drop(progTable)
      _ <- drop(clusterTable)
      _ <- drop(distTable)
    } yield (OK, "Dropped schemas".asJson)
  }

  def insertProgram(prog: Requests.ProgramInsertReq): HandleResp = {
    implicit val timeout = new Timeout(100 seconds)
    Compiler.compileWithStats(prog) match {
      case -\/(err) => // Didn't compile
        Future((BadRequest, ("Compilation failed: " ++ err.toString).asJson))
      case \/-(compiledProg) => // Compiled fine, index it
        for {
          id <- insertProgActorRef ? InsertProgMsg(
            compiledProg, prog.updateClusters.getOrElse(false))
        } yield (OK, id.asInstanceOf[Int].asJson)
    }
  }

  def deleteProgram(id: Int): HandleResp = {
    deleteById(id, progTable).map(_ match {
      case false => (NotFound, "Program ID does not exist".asJson)
      case true => (OK, "Success".asJson)
    })
  }

  def dropQuestion(quesId: String): HandleResp = {
    val myQuery = progTable.filter(_.quesId === quesId).delete.asTry.map {
      case Failure(ex) => {
        println(s"Error: ${ex.getMessage}")
        false
      }
      case Success(_) => true
    }
    for {
      result <- driver.runDB(myQuery)
    } yield if (result) {
      (OK, "Success".asJson)
    } else {
      (BadRequest, s"Could not drop the question: ${quesId}".asJson)
    }
  }
}
