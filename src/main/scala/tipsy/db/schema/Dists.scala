package tipsy.db.schema

import tipsy.db.Constraints._
import tipsy.db.TipsyPostgresProfile.api._

import scala.math._

import io.circe.syntax._

case class Dist (
  id1: Int,
  id2: Int,
  quesId: String,
  dist: Double
)

class Dists(tag: Tag) extends Table[Dist](tag, "DISTS") {
  def id1: Rep[Int] = column[Int]("ID1")
  def id2: Rep[Int] = column[Int]("ID2")
  def quesId: Rep[String] = column[String]("QUES_ID")
  def dist: Rep[Double] = column [Double]("DISTS")

  def pk = primaryKey("DIST_pkey", (id1, id2))

  def * = (id1, id2, quesId, dist) <> ((Dist.apply _).tupled, Dist.unapply)
}

object Dists {
  def createDistEntry(id1: Int, id2: Int, quesId: String, dist: Double) = {
    Dist(
      id1 = min(id1, id2),
      id2 = max(id2, id1),
      quesId = quesId,
      dist = dist
    )
  }

  def getAsJson(matrix: Seq[(Int, Int, Double)]): String = {
    matrix.asJson.toString
  }
}
