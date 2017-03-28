package tipsy.db

import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext

import slick.driver.PostgresDriver.api._
import slick.backend.DatabasePublisher
import scala.util.{ Success, Failure }

import scala.reflect._

trait Ops {
  import Constraints._
  import schema._

  implicit val driver: Driver
  implicit val executionContext: ExecutionContext

  def handleError(cmds: () => Unit) =
    try {
      cmds()
    } catch {
      case e: Throwable => println(e)
    }

  def insert[F, T <: Table[F] with WithPrimaryKey]
    (item: F, table: TableQuery[T]): Int = {

    /** InsertQuery needed to obtain the ID assigned to the new entry
      *
      * Source:
      * http://stackoverflow.com/questions/31443505/
      **/
    val insertQuery =
      (table returning table.map(_.id) into ((_, id) => id))

    driver.runDB(insertQuery += item)
  }

  def getById[F, T<:Table[F] with WithPrimaryKey]
    (id: Int, table: TableQuery[T]): Option[F] = {

    driver.runDB {
      table.filter(_.id === id).result
    }.headOption
  }

  def deleteById[T<:Table[_] with WithPrimaryKey]
    (id: Int, table: TableQuery[T]): Boolean = {

    driver.runDB {
      table.filter(_.id === id).delete.asTry.map {
        case Failure(ex) => {
          println(s"Error: ${ex.getMessage}")
          false
        }
        case Success(_) => true
      }
    }
  }

  def create[T <: Table[_]](table: TableQuery[T]): Unit = {
    driver.runDB(table.schema.create)
  }

  def drop[T <: Table[_]](table: TableQuery[T]): Unit = {
    driver.runDB(table.schema.drop)
  }
}
