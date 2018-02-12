package tipsy.db

import tipsy.db.TipsyPostgresProfile.api._
import scala.concurrent.Future

case class Driver(db: Database) {
  def close() {
    println("Shutting down DB connection")
    db.close()
  }

  def runDB[R](action: DBIOAction[R, NoStream, Nothing]): Future[R] = {
    db.run(action)
  }
}

object TipsySlick {
  val db = Database.forConfig("tipsydb")

  def apply(): Driver = {
    Driver(db)
  }
}
