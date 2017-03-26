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
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.directives.FileAndResourceDirectives

import spray.json._

import scala.io.StdIn
import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration

import slick.driver.PostgresDriver.api._
import slick.backend.DatabasePublisher

/**
  * Web: Frontend to talk to external services
  * store programs in database, and provide
  * information/corrections
  */
object Web extends JsonSupport with Ops with FileAndResourceDirectives {

  implicit val system = ActorSystem("web-tipsy")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  implicit val driver: Driver = TipsySlick()

  val progTable: TableQuery[Programs] = TableQuery[Programs]

  // modes is currently not used
  def apply(modes: Set[CLIMode]): Unit = {
    val route: Route =
      pathPrefix ("api") {

        post {
          path("submit") {
            entity(as[Requests.ProgramInsertReq]) { prog =>
              val tupProg = Program(
                0,
                prog.userId,
                System.currentTimeMillis().toString(),
                prog.quesId,
                prog.code,
                "0"
              )
              val id = insert(tupProg, progTable)
              complete(Map(
                "success" -> true.toJson,
                "id" -> id.toJson
              ))
            }
          }
        } ~ get {
          path ("createSchema") {
            create(progTable)
            complete("Created schemas")
          } ~ path ("dropSchema") {
            drop(progTable)
            complete("Deleted schemas")
          } ~ path ("progCount") {

            val myQuery: Query[Rep[Int], Int, Seq] =
              progTable.map(_.id)

            val progs = driver.runDB(myQuery.result)

            complete(Map(
              "Available programs" -> progs.toJson,
              "Count" -> progs.length.toJson
            ).toJson)
          } ~ path ("getId" / IntNumber) { id =>
            val prog: Option[Program] = driver.runDB {
              progTable.filter(_.id === id).result
            }.headOption

            complete("Found")

          } ~ path ("corrections") {
            complete("No corrections")
          }
        }

      } ~ path ("health") {
        complete("System is up")
      } ~ getFromDirectory("view")

    val bindingFuture = Http().bindAndHandle(route, "0.0.0.0", 8070)
    println(s"Server online at http://localhost:8070/\nPress RETURN to stop...")
    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => {
        driver.close()
        system.terminate()
      })
  }
}
