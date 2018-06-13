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
  implicit protected val executionContext: ExecutionContext
  implicit protected val driver: Driver = TipsySlick()

  protected val progTable: TableQuery[Programs] = TableQuery[Programs]
  protected val clusterTable: TableQuery[Clusters] = TableQuery[Clusters]
  protected val distTable: TableQuery[Dists] = TableQuery[Dists]
}

trait TipsyActors {
  implicit protected val system: ActorSystem = ActorSystem("web-tipsy")
  implicit protected val materializer: ActorMaterializer = ActorMaterializer()
  implicit protected val executionContext = system.dispatcher
  lazy val insertProgActorRef = system.actorSelection("/user/insertProgActor")
  lazy val updateClustersActorRef = system.actorSelection("/user/updateClustersActor")
}

/**
  * Web: Frontend to talk to external services
  * store programs in database, and provide
  * information/corrections
  */
object Web extends JsonSupport with Ops with FailFastCirceSupport
    with FileAndResourceDirectives with Handlers with TipsyDriver {

  def apply(implicit config: Config): Unit = {
    implicit val blockingDispatcher = system.dispatchers.lookup("tipsy-blocking-dispatcher")

    import config._

    lazy val adminErr = complete((Forbidden, "Operation only for admins"))

    val insertProgActor = if (admin)
      system.actorOf(Props(classOf[InsertProgActor]), "insertProgActor")
    else
      system.actorOf(Props.empty, "shouldNotBeUsed")

    val route: Route =
      pathPrefix ("api") {

        post {

          // handleWith will automatically convert POST body
          // to ProgramInsertReq type (parameter of insertProgram)
          path("submit") {
            if (admin) handleWith (insertProgram)
            else adminErr
          } ~ path("corrections") {
            handleWith (correctProgramFromReq)
          }

        } ~ get {

          path ("getId" / IntNumber) { id => // Retreive a program given the ID
            complete (progFromDB(id))
          } ~ path ("getCompiled" / IntNumber) { id => // Retreive a parse tree given the ID
            complete (compiledFromDB(id))
          } ~ path ("corrections" / IntNumber) { id =>
            complete (correctProgramFromDB(id))
          } ~ path ("updateClusters" / Segment) { quesId =>
            complete (updateClusterHandler(quesId))
          } ~ path ("progCount" / Segment) { quesId => // Get list of program IDs
            complete (getProgCount(quesId))
          } ~ path ("questions") { // Get list of question IDs
            complete (getQuestions)
          } ~ path ("getSimpleSolution" / Segment) { quesId =>
            complete (getSimpleSolution(quesId))
          } ~ path ("createSchema") { // Create the postgres schema
            if (admin) complete (createSchema)
            else adminErr
          } ~ path ("dropSchema") { // Drop the table schema
            if (admin) complete (dropSchema)
            else adminErr
          } ~ path ("dropQuestion" / Segment) { quesId =>
            if (admin) complete (dropQuestion(quesId))
            else adminErr
          }
        } ~ delete {
          path (IntNumber) { id =>
            if (admin) complete (deleteProgram(id))
            else adminErr
          }
        }

      } ~ path ("health") {
        complete ("System is up")
      } ~ pathEndOrSingleSlash {
        getFromFile("view/index.html")
      } ~ getFromDirectory("view")

    val bindingFuture = Http().bindAndHandle(route, host, port)
    println(s"Server online at http://${host}:${port}/")

    Await.result(system.whenTerminated, Duration.Inf)
    driver.close()
    println("Cleanup successful")
  }
}
