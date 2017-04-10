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
import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration
import scala.util.{Success, Failure}

import tipsy.db.TipsyPostgresProfile.api._
import slick.backend.DatabasePublisher

import tipsy.actors._
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

trait TipsyDriver {
  implicit val system = ActorSystem("web-tipsy")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  implicit val driver: Driver = TipsySlick()

  val progTable: TableQuery[Programs] = TableQuery[Programs]
}

/**
  * Web: Frontend to talk to external services
  * store programs in database, and provide
  * information/corrections
  */
object Web extends JsonSupport with Ops
    with FileAndResourceDirectives with TipsyDriver {

  implicit val timeout = Timeout(2.second)

  val insertActor = system.actorOf(Props[InsertActor], "insert")

  // modes is currently not used
  def apply(modes: Set[CLIMode]): Unit = {
    val route: Route =
      pathPrefix ("api") {

        post {
          path("submit") {
            // Insert program into table

            entity(as[Requests.ProgramInsertReq]) { prog =>

              complete {
                (insertActor ? (0, prog)).map {
                  case err: String => Map("success" -> false.toJson,
                    "message" -> err.toJson)
                  case id: Int => Map("success" -> true.toJson,
                    "id" -> id.toJson)
                }
              }

            }
          }

        } ~ patch {

          path (IntNumber) { id =>
            // Insert program into table

            entity(as[Requests.ProgramInsertReq]) { prog =>

              complete {
                (insertActor ? (id, prog)).map {
                  case err: String => Map("success" -> false.toJson,
                    "message" -> err.toJson)
                  case id: Int => Map("success" -> true.toJson,
                    "id" -> id.toJson)
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

          } ~ path ("corrections") {
            complete("No corrections")
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
