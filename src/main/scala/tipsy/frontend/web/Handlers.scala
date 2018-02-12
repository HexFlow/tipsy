package tipsy.frontend.web

import tipsy.db.Requests
import tipsy.compiler._
import tipsy.compare._
import tipsy.parser._
import tipsy.db._
import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._
import tipsy.frontend._

import akka.http.scaladsl.model.StatusCodes._
import scala.concurrent.Future

import io.circe.generic.auto._
import io.circe.syntax._

trait Handlers extends JsonSupport with TableHandlers with Helpers {

  def updateClusterHandler(quesId: String): HandleResp = {
    updateClustersActorRef ! quesId
    Future((OK, "Cluster update process started"))
  }

  def progFromDB(id: Int): HandleResp = {
    getFromDB(id).map(_ match {
      case None => ((NotFound, "Program not found"))
      case Some(x) => (OK, x)
    })
  }

  def compiledFromDB(id: Int): HandleResp = {
    getFromDB(id).map(_ match {
      case None => ((NotFound, "Program not found"))
      case Some(p: Program) =>
        Compiler(p.code) match {
          case Left(err) => (BadRequest, err)
          case Right(tree: ParseTree) =>
            val res = Map("tree" -> tree.toString,
              "flow" -> tree.compress.toString)
            (OK, res)
        }
    })
  }

  def correctProgramFromReq(progreq: Requests.ProgramInsertReq): HandleResp = {
    Compiler.compileWithStats(progreq) match {
      case Left(err) => Future(BadRequest, ("Compilation failed: " ++ err.toString))
      case Right(prog) => correctGivenCode(prog)
    }
  }

  def correctProgramFromDB(id: Int): HandleResp = {
    for {
      prog <- getFromDB(id)
      res <- prog match {
        case None => Future((NotFound, "Program not found".asJson))
        case Some(prog) => correctGivenCode(prog)
      }
    } yield res
  }

  private def correctGivenCode(prog: Program): HandleResp = {
    Compiler(prog.code) match {
      case Left(err) => Future((BadRequest, ("Compilation failed: " ++ err.toString)))
      case Right(mainTree: ParseTree) =>
        NormalizeParseTree(mainTree) match {
          case Left(err) => Future((BadRequest, ("Normalization failed: " ++ err.toString)))
          case Right(nc) => for {
            res <- Correct.suggestCorrections(nc)(prog.quesId)
          } yield (OK, res)
        }
    }
  }

  def getProgCount(): HandleResp = {
    val myQuery: Query[Rep[Int], Int, Seq] = progTable.map(_.id)
    for {
      progs <- driver.runDB(myQuery.result)
    } yield (OK, Map(
      "Available programs" -> progs.asJson,
      "Count" -> progs.length.asJson
    ))
  }

  def getQuestions(): HandleResp = {
    for {
      quess <- driver.runDB(progTable.map(_.quesId).distinct.result)
    } yield (OK, quess)
  }

  def getSimpleSolution(quesId: String): HandleResp = {
    for {
      prog <- driver.runDB(progTable.filter(_.quesId === quesId).take(1).result)
    } yield (prog.headOption.map(x => (OK, x.code.asJson))
      .getOrElse((NotFound, "No program was found for this question.")))
  }
}
