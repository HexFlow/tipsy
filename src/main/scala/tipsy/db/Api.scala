package tipsy.db

import com.github.tminglei.slickpg._
import slick.driver.PostgresDriver.api._
import slick.jdbc.{GetResult, PostgresProfile}
import spray.json._

trait TipsyPostgresProfile extends PostgresProfile
    with PgSprayJsonSupport
    with array.PgArrayJdbcTypes {
  override val pgjson = "jsonb"

  override val api: API = new API {}

  val plainAPI = new API with SprayJsonPlainImplicits

  trait API extends super.API with JsonImplicits {
  }
}

object TipsyPostgresProfile extends TipsyPostgresProfile
