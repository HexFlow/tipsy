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

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

trait Handlers extends Ops with TipsyDriver with JsonSupport with TableHandlers
    with Helpers {

  def similarFromDB(id: Int): HandleResp = {
    getFromDB(id) match {
      case None => ((NotFound, "Program not found".asJson))
      case Some(prog) =>
        val progs = SimilarProgs(prog)
        val res = Map("similar" -> progs.toString.asJson,
          "count" -> progs.length.asJson).asJson
        (OK, res)
    }
  }

  def progFromDB(id: Int): HandleResp = {
    getFromDB(id) match {
      case None => ((NotFound, "Program not found".asJson))
      case Some(x) => (OK, x.asJson)
    }
  }

  def compiledFromDB(id: Int): HandleResp = {
    getFromDB(id) match {
      case None => ((NotFound, "Program not found".asJson))
      case Some(p: Program) =>
        Compiler(p.code) match {
          case Left(err) => (BadRequest, err.toString.asJson)
          case Right(tree: ParseTree) =>
            val res = Map("tree" -> tree.toString.asJson,
              "flow" -> tree.compress.toString.asJson).asJson
            (OK, res)
        }
    }
  }

  def correctProgramFromReq(progreq: Requests.ProgramInsertReq): HandleResp = {
    Compiler.compileWithStats(progreq) match {
      case Left(err) => (BadRequest, ("Compilation failed: " ++ err.toString).asJson)
      case Right(prog) => correctGivenCode(prog)
    }
  }

  def correctProgramFromDB(id: Int): HandleResp = {
    getFromDB(id) match {
      case None => (NotFound, "Program not found".asJson)
      case Some(prog) => correctGivenCode(prog)
    }
  }

  private def correctGivenCode(prog: Program): HandleResp = {
    Compiler(prog.code) match {
      case Left(err) => (BadRequest, ("Compilation failed: " ++ err.toString).asJson)
      case Right(mainTree: ParseTree) =>
        val prgs = SimilarProgs(prog)

        val trees = prgs.map { x => Compiler(x.code) } collect {
          case Right(tree) => tree
        }

        val distances = LeastEdit.compareWithTrees(mainTree, trees)
          .sortWith(_._2 > _._2)

        val corrections = distances collect {
          case (correctorTree, dist) => Correct(mainTree, correctorTree)
        }

        val res = corrections.map {
          case Left(err) =>
            Map("success" -> false.asJson, "error" -> err.toString.asJson).asJson
          case Right(corrs) =>
            Map("success" -> true.asJson,
              "corrections" -> corrs.map { x =>
                Map("name" -> x._1.asJson,
                  "change" -> x._2.asJson).asJson
              }.asJson,
              "count" -> corrections.length.asJson).asJson
        }.asJson

        (OK, res)
    }
  }

  def getProgCount(): HandleResp = {
    val myQuery: Query[Rep[Int], Int, Seq] = progTable.map(_.id)
    val progs = driver.runDB(myQuery.result)
    val res = Map(
      "Available programs" -> progs.asJson,
      "Count" -> progs.length.asJson
    ).asJson
    (OK, res)
  }
}
