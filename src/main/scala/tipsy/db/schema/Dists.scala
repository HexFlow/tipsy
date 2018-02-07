package tipsy.db.schema

import tipsy.db.Constraints._
import tipsy.db.TipsyPostgresProfile.api._

import io.circe.syntax._

case class Dist (
  id: Int,
  quesId: String,
  dists: Map[Int, Double]
)

class Dists(tag: Tag) extends Table[Dist](tag, "DISTS") with WithPrimaryKey {
  def id: Rep[Int] = column[Int]("DIST_ID")
  def quesId: Rep[String] = column[String]("QUES_ID")
  def dists: Rep[Map[Int, Double]] = column [Map[Int, Double]]("DISTS")

  def pk = primaryKey("DIST_pkey", (id, quesId))

  def * = (id, quesId, dists) <> ((Dist.apply _).tupled, Dist.unapply)
}

object Dists {
  def getAsDump[A](matrix: Seq[(A, Map[A, Double])]): String = {
    matrix.map {
      case (id, distMap) => s"${id}: " ++
        distMap.toList.map {
          case (nid, dist) => s"(${nid}, ${dist})"
        }.mkString(" | ")
    }.mkString("\n")
  }

  def getAsJson(matrix: Map[String, (Int, Map[String, Double])]): String = {
    matrix.asJson.toString
  }
}
