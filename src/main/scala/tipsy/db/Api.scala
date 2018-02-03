package tipsy.db

import com.github.tminglei.slickpg._
import slick.driver.PostgresDriver.api._
import slick.jdbc.{GetResult, PostgresProfile}

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

import schema.Stats
import tipsy.parser.CFEnum
import scala.util.parsing.input.{ NoPosition, OffsetPosition }

trait TipsyPostgresProfile extends PostgresProfile
    with PgCirceJsonSupport
    with PgArraySupport
    with array.PgArrayJdbcTypes {
  override val pgjson = "jsonb"

  override val api = MyAPI

  object MyAPI extends API
      with JsonImplicits
      with ArrayImplicits {

    import tipsy.parser.CFEnum

    implicit val statsColumnType =
      MappedColumnType.base[Stats, Json](
        { s => s.asJson },
        { j => decode[Stats](j.toString).right.get }
      )

    // This following part is still a dream. It did not work since
    // NoPosition cannot be serialized somehow. Will wait till
    // all NoPositions are eliminated :)
    import java.util.Base64
    import java.io._

    def cfToStr(s: CFEnum) = {
      val res = new ByteArrayOutputStream()
      val oos = new ObjectOutputStream(res)
      oos.writeObject(s)
      oos.close
      new String(Base64.getEncoder().encode(res.toByteArray()))
    }

    def strToCf(j: String) = {
      val bin = Base64.getDecoder().decode(j.getBytes())
      val ois = new ObjectInputStream(new ByteArrayInputStream(bin))
      val res = ois.readObject.asInstanceOf[CFEnum]
      ois.close
      res
    }

    implicit val cfColumnType =
      MappedColumnType.base[CFEnum, String](
        { s => cfToStr(s) },
        { j => strToCf(j) }
      )

    implicit val cfListColumnType =
      MappedColumnType.base[List[CFEnum], Json](
        { s => s.map(cfToStr(_)).asJson },
        { j => decode[List[String]](j.toString).right.get.map(strToCf(_)) }
      )
  }
}

object TipsyPostgresProfile extends TipsyPostgresProfile
