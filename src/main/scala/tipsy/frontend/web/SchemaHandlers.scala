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

import scala.util.{Success, Failure}

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

trait TableHandlers extends Ops with TipsyDriver with Helpers {

  def createSchema(): HandleResp = {
    create(progTable)
    (OK, "Created schemas".asJson)
  }

  def dropSchema(): HandleResp = {
    drop(progTable)
    (OK, "Dropped schemas".asJson)
  }

  def insertProgram(prog: Requests.ProgramInsertReq): HandleResp = {
    Compiler.compileWithStats(prog) match {
      case Left(err) => // Didn't compile
        (BadRequest, ("Compilation failed: " ++ err.toString).asJson)
      case Right(compiledProg) => // Compiled fine, index it
        (OK, insertProg(compiledProg).asJson)
    }
  }

  def deleteProgram(id: Int): HandleResp = {
    deleteById(id, progTable) match {
      case false => (NotFound, "Program ID does not exist".asJson)
      case true => (OK, "Success".asJson)
    }
  }

  def dropQuestion(quesId: String): HandleResp = {
    val myQuery = progTable.filter(_.quesId === quesId).delete.asTry.map {
      case Failure(ex) => {
        println(s"Error: ${ex.getMessage}")
        false
      }
      case Success(_) => true
    }
    val result = driver.runDB(myQuery)
    if (result) {
      (OK, "Success".asJson)
    } else {
      (BadRequest, s"Could not drop the question: ${quesId}".asJson)
    }
  }
}
