package tipsy.db

import slick.driver.PostgresDriver.api._
import org.postgresql.ds.PGSimpleDataSource
import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration

case class Driver(
  db: Database
) {
  def close() {
    println("Shutting down DB connection")
    db.close()
  }

  def runDB[R](action: DBIOAction[R, NoStream, Nothing]): R = {
    val futureAction = db.run(action)
    Await.result(futureAction, Duration.Inf)
  }
}

object TipsySlick {
  val db = Database.forConfig("tipsydb")

  def apply(): Driver = {
    Driver(db)
  }
}
