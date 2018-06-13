package tipsy.frontend.web

import scalaz._
import tipsy.db.Requests
import tipsy.compiler._
import tipsy.compare._
import tipsy.parser._
import tipsy.db._
import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._
import tipsy.frontend._
import akka.http.scaladsl.model.StatusCodes._
import scala.concurrent.{ Future, Await }
import scala.concurrent.duration._
import io.circe.generic.auto._
import io.circe.syntax._

trait Handlers extends JsonSupport with TableHandlers with Helpers with ClusterActions {

  def updateClusterHandler(quesId: String): HandleResp = {
    val action = for {
      matrix <- driver.runDB {
        distTable.filter(_.quesId === quesId).map(e => (e.id1, e.id2, e.dist)).result
      }
      _ <- doUpdateClusters(matrix, quesId)
    } yield ()
    Await.result(action, Duration.Inf)
    Future((OK, "Clusters updated"))
  }

  def progFromDB(id: Int): HandleResp = {
    getFromDB(id).map(_ match {
      case None => ((NotFound, "Program not found"))
      case Some(x) => (OK, x)
    })
  }

  def compiledFromDB(id: Int): HandleResp = {
    getFromDB(id).map(_ match {
      case None => (NotFound, "Program not found")
      case Some(p: Program) =>
        Compiler(p.code) match {
          case -\/(err) => (BadRequest, err)
          case \/-(tree: ParseTree) =>
            val res = Map("tree" -> tree.toString,
              "flow" -> tree.compress.toString)
            (OK, res)
        }
    })
  }

  def correctProgramFromReq(progreq: Requests.ProgramInsertReq): HandleResp = {
    Compiler.compileWithStats(progreq) match {
      case -\/(err) => Future(BadRequest, ("Compilation failed: " ++ err.toString))
      case \/-(prog) => correctGivenCode(prog)
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
      case -\/(err) => Future((BadRequest, ("Compilation failed: " ++ err.toString)))
      case \/-(mainTree: ParseTree) =>
        NormalizeParseTree(mainTree) match {
          case -\/(err) => Future((BadRequest, ("Normalization failed: " ++ err.toString)))
          case \/-(nc) => for {
            res <- Correct.suggestCorrections(nc)(prog.quesId)
          } yield (OK, res)
        }
    }
  }

  def getProgCount(quesId: String): HandleResp = {
    for {
      len <- driver.runDB(
        progTable.filter(_.quesId === quesId).map(_.id).length.result
      )
    } yield (OK, len)
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
