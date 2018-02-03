package tipsy.frontend

import tipsy.compiler._
import tipsy.compare._
import tipsy.parser._
import tipsy.db._
import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.directives.FileAndResourceDirectives

import slick.backend.DatabasePublisher

import de.heikoseeberger.akkahttpcirce._
import io.circe.syntax._, io.circe.generic.auto._

import scala.util.{Success, Failure}
import scala.io.StdIn

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
object Web extends JsonSupport with Ops with FailFastCirceSupport
    with FileAndResourceDirectives with TipsyDriver {

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
                    val id = insertProg(compiledProg)
                    Map("success" -> true.asJson, "id" -> id.asJson)
                  case Left(err) =>
                    // Didn't compile
                    Map("success" -> false.asJson, "error" -> err.toString.asJson)
                }
              }
            }
          } ~ path("corrections") {

            entity(as[Requests.ProgramInsertReq]) { progreq =>
              Compiler.compileWithStats(progreq) match {
                case Left(err) =>
                  // Didn't compile
                  complete {
                    Map("success" -> false.asJson, "error" -> err.toString.asJson)
                  }

                case Right(prog) =>
                  Compiler(prog.code) match {
                    case Right(mainTree: ParseTree) =>
                      val prgs = SimilarProgs(prog)

                      val trees = prgs.map { x => Compiler(x.code) }.collect {
                        case Right(tree) => tree
                      }

                      val distances =
                        LeastEdit.compareWithTrees(mainTree, trees)
                          .sortWith(_._2 > _._2)

                      val corrections = distances collect {
                        case (correctorTree, dist) =>
                          Correct(mainTree, correctorTree)
                      }

                      val res = corrections.map {
                        case Left(err) =>
                          Map("success" -> false.asJson, "error" -> err.asJson)
                        case Right(corrs) =>
                          Map("success" -> true.asJson,
                            "corrections" -> corrs.map { x =>
                              Map("name" -> x._1.asJson,
                                "change" -> x._2.asJson)
                            }.asJson,
                            "count" -> corrections.length.asJson)
                      }
                      complete(res)

                    case Left(_) => ???
                  }
              }
            }
          }
        } ~ get {
          path ("draw_graph" / Segment) { quesId =>
            val progs = driver.runDB {
              progTable.filter(_.quesId === quesId).result
            }.take(50)
            val validTrees = progs.map(prog => (Compiler(prog.code), (prog.id.toString + "-" + prog.userId))).collect { case (Right(x), y) => (x, y) }.toList
            DistanceDraw(LeastEdit(validTrees.map(_._1), false), validTrees.length, validTrees.map(_._2))

            complete { "OK" }
          } ~ path ("createSchema") {
            // Create the postgres schema

            create(progTable)
            complete("Created schemas")

          } ~ path ("dropSchema") {
            // Drop the table schema

            drop(progTable)
            complete("Deleted schemas")

          } ~ path ("dropQuestion" / Segment) { quesId =>
            val myQuery = progTable.filter(_.quesId === quesId).delete.asTry.map {
              case Failure(ex) => {
                println(s"Error: ${ex.getMessage}")
                false
              }
              case Success(_) => true
            }
            val result = driver.runDB(myQuery)
            complete(Map(
              "success" -> result.asJson
            ).asJson)
          } ~ path ("progCount") {
                // Get list of program IDs

            val myQuery: Query[Rep[Int], Int, Seq] =
              progTable.map(_.id)

            val progs = driver.runDB(myQuery.result)

            complete(Map(
              "Available programs" -> progs.asJson,
              "Count" -> progs.length.asJson
            ).asJson)

          } ~ path ("getId" / IntNumber) { id =>
            // Retreive a program given the ID

            val prog: Option[Program] = driver.runDB {
              progTable.filter(_.id === id).result
            }.headOption

            prog match {
              case Some(x) => complete(x.asJson)
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
                      Map("success" -> false.asJson,
                        "message" -> err.toString.asJson)
                    case Right(tree: ParseTree) =>
                      Map("success" -> true.asJson,
                        "tree" -> tree.toString.asJson,
                        "flow" -> tree.compress.toString.asJson)
                  }
                }

                case None => ((NotFound, "Program not found"))
              }
            }

          } ~ path ("similar" / IntNumber) { id =>

            val progopt: Option[Program] = driver.runDB {
              progTable.filter(_.id === id).result
            }.headOption

            progopt match {
              case None => complete((NotFound, "Program not found"))
              case Some(prog) =>
                complete {
                  val progs = SimilarProgs(prog)
                  Map("success" -> true.asJson,
                    "similar" -> progs.toString.asJson,
                    "count" -> progs.length.asJson)
                }
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
                    val prgs = SimilarProgs(prog)

                    val trees = prgs.map { x => Compiler(x.code) }.collect {
                      case Right(tree) => tree
                    }

                    val corrections = LeastEdit.compareWithTrees(mainTree, trees).map {
                      case (correctorTree, dist) =>
                        (Correct(mainTree, correctorTree), dist)
                    }.reduce { (x, y) =>
                      if (x._2 > y._2) x
                      else y
                    }

                    corrections._1 match {
                      case Left(err) =>
                        Map("success" -> false.asJson, "error" -> err.asJson)
                      case Right(corrs) =>
                        Map("success" -> true.asJson,
                          "corrections" -> corrs.map { x =>
                            Map("name" -> x._1.asJson,
                              "change" -> x._2.asJson)
                          }.asJson,
                          "dist" -> corrections._2.asJson)
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

    StdIn.readLine()

    // TODO: This still does not shutdown the cluster, and keeps resources stuck.
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => {
        println("Shutting down HTTP")
        Http().shutdownAllConnectionPools()
        driver.close()
        println("Shutting down actor system")
        system.terminate()
        println("Cleanup successful")
      })
  }

  // Inserts provided program into database, or updates existing program.
  def insertProg(prog: Program) = {
    // Operation depends on whether an ID was provided
    val id: Int = prog.id match {
      case 0 => {
        println("Inserting into a new row")
        insert(prog, progTable)
      }
      case idReq => {
        println("Updating id: " + idReq)
        driver.runDB { progTable.insertOrUpdate(prog) }
        idReq
      }
    }

    // Return the ID to sender parent
    id
  }
}
