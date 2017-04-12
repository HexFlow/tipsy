package tipsy.frontend

import tipsy.compiler._
import tipsy.compare._
import tipsy.parser._
import tipsy.db._
import tipsy.db.schema._

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.directives.FileAndResourceDirectives

import spray.json._

import scala.io.StdIn
import scala.concurrent.{Future, Await, ExecutionContext}
import scala.concurrent.duration.Duration
import scala.util.{Success, Failure}

import tipsy.db.TipsyPostgresProfile.api._
import slick.backend.DatabasePublisher

import tipsy.actors._
import tipsy.actors.Messages._

import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

trait TipsyDriver {
  implicit val system = ActorSystem("web-tipsy")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  implicit val driver: Driver = TipsySlick()

  implicit val timeout = Timeout(3.second)

  val progTable: TableQuery[Programs] = TableQuery[Programs]
}

/**
  * Web: Frontend to talk to external services
  * store programs in database, and provide
  * information/corrections
  */
object Web extends JsonSupport with Ops
    with FileAndResourceDirectives with TipsyDriver {

  val insertActor = system.actorOf(Props[InsertActor], "insert")
  val similarActor = system.actorOf(Props[SimilarActor], "similar")

  // modes is currently not used
  def apply(modes: Set[CLIMode]): Unit = {
    val route: Route =
      pathPrefix ("api") {

        post {
          path("submit") {
            // Insert program into table

            entity(as[Requests.ProgramInsertReq]) { prog =>

              complete {
                Compiler.compileWithStats(prog) match {
                  case Right(compiledProg) =>
                    // Compiled fine, index it
                    for {
                      InsertResp(id) <- (insertActor ? InsertReq(compiledProg))
                    } yield Map("success" -> true.toJson, "id" -> id.toJson)
                  case Left(err) =>
                    // Didn't compile
                    Map("success" -> false.toJson, "error" -> err.toString.toJson)
                }
              }
            }
          }
        } ~ get {
          path ("createSchema") {
            // Create the postgres schema

            create(progTable)
            complete("Created schemas")

          } ~ path ("dropSchema") {
            // Drop the table schema

            drop(progTable)
            complete("Deleted schemas")

          } ~ path ("progCount") {
            // Get list of program IDs

            val myQuery: Query[Rep[Int], Int, Seq] =
              progTable.map(_.id)

            val progs = driver.runDB(myQuery.result)

            complete(Map(
              "Available programs" -> progs.toJson,
              "Count" -> progs.length.toJson
            ).toJson)

          } ~ path ("getId" / IntNumber) { id =>
            // Retreive a program given the ID

            val prog: Option[Program] = driver.runDB {
              progTable.filter(_.id === id).result
            }.headOption

            prog match {
              case Some(x) => complete(x.toJson)
              case None => complete((NotFound, "Program not found"))
            }

          } ~ path ("getCompiled" / IntNumber) { id =>
            // Retreive a parse tree given the ID

            val prog: Option[Program] = driver.runDB {
              progTable.filter(_.id === id).result
            }.headOption

            complete {
              prog match {
                case Some(p: Program) => {
                  Compiler(p.code) match {
                    case Left(err) =>
                      Map("success" -> false.toJson,
                        "message" -> err.toString.toJson)
                    case Right(tree: ParseTree) =>
                      Map("success" -> true.toJson,
                        "tree" -> tree.toString.toJson,
                        "flow" -> tree.compress.toString.toJson)
                  }
                }

                case None => ((NotFound, "Program not found"))
              }
            }

          } ~ path ("similar" / IntNumber) { id =>

            complete {
              for {
                SimilarCheckResp(progs) <-(similarActor ? SimilarCheck(id))
              } yield Map("success" -> true.toJson,
                "similar" -> progs.toString.toJson,
                "count" -> progs.length.toJson)
            }

          } ~ path ("corrections" / IntNumber) { id =>

            val progopt: Option[Program] = driver.runDB {
              progTable.filter(_.id === id).result
            }.headOption

            progopt match {
              case None => complete ((NotFound, "Program not found"))

              case Some(prog) =>
                val res = Compiler(prog.code) match {
                  case Right(mainTree: ParseTree) =>
                    for {
                      SimilarCheckResp(progs) <- (similarActor ? SimilarCheck(id))
                    } yield {

                      val trees = progs.map { x => Compiler(x.code) }.collect {
                        case Right(tree) => tree
                      }

                      val distances =
                        LeastEdit.compareWithTrees(mainTree, trees)
                          .sortWith(_._2 > _._2)

                      val corrections = distances collect {
                        case (correctorTree, dist) if dist > 5 =>
                          Correct(mainTree, correctorTree)
                      }

                      Map("success" -> true.toJson,
                        "corrections" -> corrections.toString.toJson,
                        "count" -> corrections.length.toJson)
                    }
                  case Left(_) => ???
                }

                complete(res)
            }
          }
        } ~ delete {

          path (IntNumber) { id =>
            deleteById(id, progTable) match {
              case false => complete((NotFound, "Program ID does not exist"))
              case true => complete("Success")
            }
          }

        }

      } ~ path ("health") {
        complete("System is up")
      } ~ getFromDirectory("view")

    val bindingFuture = Http().bindAndHandle(route, "0.0.0.0", 8070)
    println(s"Server online at http://localhost:8070/")

    sys.addShutdownHook {
      driver.close()
      system.terminate()
      ()
    }
  }
}
