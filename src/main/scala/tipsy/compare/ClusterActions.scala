package tipsy.compare

import scala.concurrent.Future

import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._
import tipsy.frontend._

import io.circe.parser._

/** Actions concerning clusters of questions.
  */
trait ClusterActions extends TipsyDriver {

  private var cnt = 0

  private def mean(l: List[Double]) = {
    l.sum / l.length
  }
  private def sqr(x: Double) = {
    x*x
  }

  /** Returns the variance of the input values.
    * @param l List of Double values whose variance is required.
    */
  def getVariance(l: List[Double]) = {
    scala.math.sqrt(mean(l.map(sqr)) - sqr(mean(l)))
  }

  /** Returns a Future of the variance of the requested question ID.
    * @param quesId Question ID whose programs' scores' variance is required.
    */
  def findVarianceOfQues(quesId: String): Future[Double] = {
    for {
      progScores <- driver.runDB {
        progTable.filter(_.quesId === quesId).map(_.score).result
      }.map(_.map(_.toDouble))
    } yield getVariance(progScores.toList)
  }

  /** Consumes a 2D matrix of the program distances, belonging to a question,
    * and updates the cluster of that question in the DB using `hierarchical_clustering.py`
    * script.
    * @param matrix Matrix containing distances between the program IDs.
    * @param quesId The question ID in question.
    */
  def doUpdateClusters(matrix: Seq[(Int, Int, Double)], quesId: String): Future[Unit] = {
    cnt = cnt + 1

    val matrixStr = Dists.getAsJson(matrix)

    val res = SimpleTcpClient(matrixStr)

    decode[List[List[Int]]](res) match {
      case Left(err) =>
        println("Error in parsing clustering response.")
        println("Error was: " ++ err.toString)
        println("Output was: " ++ res)
        Future()
      case Right(clusterList) =>
        for {
          _ <- driver.runDB {
            clusterTable.insertOrUpdate(Cluster(
              quesId = quesId,
              cluster = clusterList
            ))
          }
          _ <- Future(println(s"Finished updating clusters."))
        } yield ()
    }
  }
}
