package tipsy.frontend.web

import tipsy.frontend.Requests
import tipsy.compiler._
import tipsy.compare._
import tipsy.parser._
import tipsy.db._
import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._
import tipsy.frontend._

import akka.http.scaladsl.model.StatusCodes._
import scala.concurrent.Future

import scala.util.{Success, Failure}

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

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
    Compiler.compileWithStats(prog) match {
      case Left(err) => // Didn't compile
        Future((BadRequest, ("Compilation failed: " ++ err.toString).asJson))
      case Right(compiledProg) => // Compiled fine, index it
        for {
          id <- insertProg(compiledProg, prog.updateClusters.getOrElse(false))
        } yield (OK, id.asJson)
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
