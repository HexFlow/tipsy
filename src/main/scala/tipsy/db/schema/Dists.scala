package tipsy.db.schema

import tipsy.db.Constraints._
import tipsy.db.TipsyPostgresProfile.api._

case class Dist (
  id: Int,
  quesId: String,
  dists: Map[Int, Double]
)

class Dists(tag: Tag) extends Table[Dist](tag, "DISTS") with WithPrimaryKey {
  def id: Rep[Int] = column[Int]("DIST_ID", O.PrimaryKey)
  def quesId: Rep[String] = column[String]("QUES_ID")
  def dists: Rep[Map[Int, Double]] = column [Map[Int, Double]]("DISTS")

  def * = (id, quesId, dists) <> ((Dist.apply _).tupled, Dist.unapply)
}
