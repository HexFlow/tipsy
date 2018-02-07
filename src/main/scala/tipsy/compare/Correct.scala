package tipsy.compare

import tipsy.frontend._
import tipsy.db._
import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._

import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.collection.parallel.immutable.ParVector

object Correct extends TipsyDriver with Ops with TipsyActors {
  val repCntInCluster = 2

  def suggestCorrections(code: NormCode)(implicit quesId: String): Future[List[EditRet]] = {
    for {
      repIds <- fetchClusterRepIds
      minDistRepIdsAndEditRet <- getBestN(code, 4, repIds)
      progIdsToConsider <- Future.sequence(
        minDistRepIdsAndEditRet.map(x => fetchClusterFromRepId(x._1))
      )
      minDistEditRet <- getBestN(code, 4, progIdsToConsider.flatten.distinct).map(_.map(_._2))
    } yield minDistEditRet
  }

  def getBestN(code: NormCode, n: Int, ids: List[Int]): Future[List[(Int, EditRet)]] = {
    for {
      progs <- Future.sequence(ids.map (getProgOfId(_)))
      idCumNCList = progs.collect { case Some(x) => (x.id, x.cf) }
    } yield
      idCumNCList.toVector.par.map {
        case (id, ref) => (id, Compare.findDist(code, ref))
      }.toList.sortWith(_._2 <= _._2).take(n)
  }

  def getProgOfId(id: Int): Future[Option[Program]] = {
    driver.runDB {
      progTable.filter(_.id === id).result
    }.map(_.headOption)
  }

  def fetchClusterRepIds(implicit quesId: String): Future[List[Int]] = {
    // Fetch clusters of this question. Take `repCntInCluster` from each and return that list.
    for {
      cluster <- driver.runDB {
        clusterTable.filter(_.quesId === quesId).result
      }.map(_.headOption.map(_.cluster).getOrElse(List()))
    } yield cluster.flatMap(_.take(repCntInCluster))
  }

  def fetchClusterFromRepId(id: Int)(implicit quesId: String): Future[List[Int]] = {
    // Given a representative of a cluster, fetch that full cluster.
    for {
      cluster <- driver.runDB {
        clusterTable.filter(_.quesId === quesId).result
      }.map(_.headOption.map(_.cluster).getOrElse(List()))
    } yield
      cluster.collect {
        case x if x.take(repCntInCluster).contains(id) => x
      }.headOption.getOrElse(List())
  }
}
