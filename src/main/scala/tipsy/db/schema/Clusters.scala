package tipsy.db.schema

import tipsy.db.Constraints._
import tipsy.db.TipsyPostgresProfile.api._

case class Cluster (
  quesId: String,
  cluster: List[List[Int]]
)

class Clusters(tag: Tag) extends Table[Cluster](tag, "CLUSTERS") {
  def id: Rep[String] = column[String]("QUES_ID", O.PrimaryKey)
  def cluster: Rep[List[List[Int]]] = column [List[List[Int]]]("CLUSTER")

  def * = (id, cluster) <> ((Cluster.apply _).tupled, Cluster.unapply)
}
