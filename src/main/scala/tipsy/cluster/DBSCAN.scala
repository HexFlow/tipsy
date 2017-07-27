package tipsy.cluster

import scala.collection.mutable.{Map => mMap, Queue}

sealed trait DBSCANPoint
case object NOISE extends DBSCANPoint
case object CORE extends DBSCANPoint
case object BORDER extends DBSCANPoint

object DBSCAN {

  var identifier: mMap[Int, (DBSCANPoint, Int)] = mMap[Int, (DBSCANPoint, Int)]()
  var cluster = -1
  var matrixNetwork: List[List[Double]] = List[List[Double]]()
  def apply(tMatrixNetwork: List[List[Double]], length: Int, eps: Double, minPts: Int): mMap[Int, (DBSCANPoint, Int)] = {
    cluster = -1
    identifier = mMap[Int, (DBSCANPoint, Int)]()
    matrixNetwork = normalize(tMatrixNetwork)
    for (i <- List.range(0, length)) {
      identifier get i match {
        case Some(_) =>
        case None => {
          val neighbours = regionQuery(length, i, eps)
          if (neighbours.length < minPts) {
            identifier(i) = (NOISE, -1)
          } else {
            cluster += 1
            expandCluster(length, i, eps, neighbours, minPts)
          }
        }
      }
    }

    identifier
  }

  def normalize[A: Numeric](matrix: List[List[A]]): List[List[A]] = implicitly[Numeric[A]] match {
    case num: Fractional[_] => {
      import num._
      val mx = matrix.map(_.reduceLeft(abs(_) max abs(_))).max
      matrix.map(_.map(_/mx))
    }
    case num: Integral[_] => {
      import num._
      val mx = matrix.map(_.reduceLeft(abs(_) max abs(_))).max
      matrix.map(_.map(_/mx))
    }
    case _ => sys.error("Undivisable Number")
  }

  def expandCluster(length: Int, ind: Int, eps: Double, neighbours: List[Int], minPts: Int): Unit = {
    identifier(ind) = (CORE, cluster)
    val clusterQueue = Queue[Int]()
    clusterQueue ++= neighbours
    while (! clusterQueue.isEmpty) {
      val neighbour = clusterQueue.dequeue
      identifier get neighbour match {
        case Some((NOISE, -1)) => {
          identifier(neighbour) = (BORDER, cluster)
        }
        case None => {
          identifier(neighbour) = (BORDER, cluster)
          val newNeighbours = regionQuery(length, neighbour, eps)
          if (newNeighbours.length >= minPts) {
            clusterQueue ++= newNeighbours
            identifier(neighbour) = (CORE, cluster)
          }
        }
        case _ =>
      }
    }
  }

  def regionQuery(length: Int, ind: Int, eps: Double): List[Int] = {
    matrixNetwork(ind).zipWithIndex.filter(_._1 < eps).map(_._2)
  }
}
