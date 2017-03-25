package tipsy.db

import slick.driver.PostgresDriver.api._
import schema._

/**
  * Define all generic methods on tables here
  */
object Constraints {
  import annotation.implicitNotFound

  /**
    * WithPrimaryKey is implemented for those classes which need a
    * getId function defined for them. In that case, getId would
    * provide the value of the primary key in that class
    */
  @implicitNotFound("No member of type class WithPrimaryKey in scope for ${T}")
  trait WithPrimaryKey[T] {
    def getId(x: T): Rep[Int]
  }

  object WithPrimaryKey {
    implicit object ProgramsPrimKey extends WithPrimaryKey[Programs] {
      def getId(x: Programs): Rep[Int] = {
        x.id
      }
    }
  }
}
