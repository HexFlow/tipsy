package tipsy.db

import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration

import slick.driver.PostgresDriver.api._
import slick.backend.DatabasePublisher

import scala.reflect._

trait Ops {
  implicit val driver: Driver

  def handleError(cmds: () => Unit) =
    try {
      cmds()
    } catch {
      case e: Throwable => println(e)
    }

  def insert[F, T <: Table[F]](item: F, table: TableQuery[T]): Unit = {
    driver.runDB(table += item)
  }

  def create[T <: Table[_]](table: TableQuery[T]): Unit = {
    driver.runDB(table.schema.create)
  }

  def drop[T <: Table[_]](table: TableQuery[T]): Unit = {
    driver.runDB(table.schema.drop)
  }
}
