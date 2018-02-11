package tipsy.frontend

import tipsy.actors._
import tipsy.db._
import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._
import tipsy.frontend.web.Handlers

import akka.actor.ActorSystem
import akka.actor._
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.http.scaladsl.model.StatusCodes.Forbidden
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.directives.FileAndResourceDirectives

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import de.heikoseeberger.akkahttpcirce._
import io.circe.generic.auto._


trait TipsyDriver {
  implicit val executionContext: ExecutionContext
  implicit val driver: Driver = TipsySlick()

  val progTable: TableQuery[Programs] = TableQuery[Programs]
  val clusterTable: TableQuery[Clusters] = TableQuery[Clusters]
  val distTable: TableQuery[Dists] = TableQuery[Dists]
}

trait TipsyActors {
  implicit val system: ActorSystem = ActorSystem("web-tipsy")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
  val insertProgActorRef = system.actorSelection("/user/insertProgActor")
  val updateClustersActorRef = system.actorSelection("/user/updateClustersActor")
}

/**
  * Web: Frontend to talk to external services
  * store programs in database, and provide
  * information/corrections
  */
object Web extends JsonSupport with Ops with FailFastCirceSupport
    with FileAndResourceDirectives with Handlers with TipsyDriver {

  val insertProgActor = system.actorOf(Props(classOf[InsertProgActor]), "insertProgActor")

  def apply(config: Config): Unit = {
    implicit val blockingDispatcher = system.dispatchers.lookup("tipsy-blocking-dispatcher")

    val route: Route =
      pathPrefix ("api") {

        post {
          // entity is post body.
          entity(as[Requests.ProgramInsertReq]) { prog =>
            path("submit") {
              complete (insertProgram(prog))
            } ~ path("corrections") {
              complete (correctProgramFromReq(prog))
            }
          }
        } ~ get {

          path ("getId" / IntNumber) { id => // Retreive a program given the ID
            complete (progFromDB(id))
          } ~ path ("getCompiled" / IntNumber) { id => // Retreive a parse tree given the ID
            complete (compiledFromDB(id))
          } ~ path ("corrections" / IntNumber) { id =>
            complete (correctProgramFromDB(id))
          } ~ path ("createSchema") { // Create the postgres schema
            if (config.admin) complete (createSchema())
            else complete((Forbidden, "Are you lost?"))
          } ~ path ("dropSchema") { // Drop the table schema
            if (config.admin) complete (dropSchema())
            else complete((Forbidden, "Are you lost?"))
          } ~ path ("dropQuestion" / Segment) { quesId =>
            if (config.admin) complete (dropQuestion(quesId))
            else complete((Forbidden, "Are you lost?"))
          } ~ path ("updateClusters" / Segment) { quesId =>
            complete (updateClusterHandler(quesId))
          } ~ path ("progCount") { // Get list of program IDs
            complete (getProgCount())
          }
        } ~ delete {
          path (IntNumber) { id =>
            if (config.admin) complete (deleteProgram(id))
            else complete((Forbidden, "Are you lost?"))
          }
        }

      } ~ path ("health") {
        complete ("System is up")
      } ~ getFromDirectory("view")

    val bindingFuture = Http().bindAndHandle(route, config.host, config.port)
    println(s"Server online at http://${config.host}:${config.port}/")

    Await.result(system.whenTerminated, Duration.Inf)
    driver.close()
    println("Cleanup successful")
  }
}
