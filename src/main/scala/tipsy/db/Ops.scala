package tipsy.db

import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration

import slick.driver.PostgresDriver.api._
import slick.backend.DatabasePublisher

import scala.reflect._

trait Ops {
  import Constraints._
  import schema._

  implicit val driver: Driver

  def handleError(cmds: () => Unit) =
    try {
      cmds()
    } catch {
      case e: Throwable => println(e)
    }

  def insert[F, T <: Table[F]: WithPrimaryKey]
    (item: F, table: TableQuery[T]): Int = {

    /** InsertQuery needed to obtain the ID assigned to the new entry
      *
      * Read this article to understand typeclasses
      * http://danielwestheide.com/blog/2013/02/06
      * /the-neophytes-guide-to-scala-part-12-type-classes.html
      *
      * Basically, this insert will work only on those types
      * which have a primary key (getId function) defined on them
      *
      * Source:
      * http://stackoverflow.com/questions/31443505/
      **/
    val insertQuery =
      (table returning table.map(implicitly[WithPrimaryKey[T]].getId(_))
        into ((_, id) => id))

    driver.runDB(insertQuery += item)
  }

  def create[T <: Table[_]](table: TableQuery[T]): Unit = {
    driver.runDB(table.schema.create)
  }

  def drop[T <: Table[_]](table: TableQuery[T]): Unit = {
    driver.runDB(table.schema.drop)
  }
}
