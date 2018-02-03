package tipsy.db

import com.github.tminglei.slickpg._
import slick.driver.PostgresDriver.api._
import slick.jdbc.{GetResult, PostgresProfile}

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

import schema.Stats

trait TipsyPostgresProfile extends PostgresProfile
    with PgCirceJsonSupport
    with PgArraySupport
    with array.PgArrayJdbcTypes {
  override val pgjson = "jsonb"

  override val api = MyAPI

  object MyAPI extends API
      with JsonImplicits
      with ArrayImplicits {

    implicit val statsColumnType =
      MappedColumnType.base[Stats, Json](
        { s => s.asJson },
        { j => decode[Stats](j.toString).right.get }
      )
  }
}

object TipsyPostgresProfile extends TipsyPostgresProfile
