package tipsy.db.schema

import tipsy.db.Constraints._
import tipsy.db.TipsyPostgresProfile.api._

case class Cluster (
  id: Int,
  quesId: String,
  cluster: List[List[Int]]
)

class Clusters(tag: Tag) extends Table[Cluster](tag, "CLUSTERS") with WithPrimaryKey {
  def id: Rep[Int] = column[Int]("CL_ID", O.PrimaryKey, O.AutoInc)
  def quesId: Rep[String] = column[String]("QUES_ID")
  def cluster: Rep[List[List[Int]]] = column [List[List[Int]]]("CLUSTER")

  def * = (id, quesId, cluster) <> ((Cluster.apply _).tupled, Cluster.unapply)
}
