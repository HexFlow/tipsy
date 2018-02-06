package tipsy.db

import tipsy.db.TipsyPostgresProfile.api._

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
