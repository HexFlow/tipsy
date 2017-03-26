package tipsy.db

import slick.driver.PostgresDriver.api._
import schema._

/**
  * Define all generic methods on tables here
  */
object Constraints {
  /**
    * WithPrimaryKey is implemented for those classes which need a
    * id function defined for them.
    */
  trait WithPrimaryKey {
    def id(): Rep[Int]
  }
}
