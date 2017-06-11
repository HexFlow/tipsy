package tipsy.cluster

import scala.util.Random

object KMeans {

  var coordinates: List[List[Double]] = List[List[Double]]()
  var centroids: Array[Array[Double]] = Array[Array[Double]]()
  var cluster: Array[Int] = Array[Int]()
  var dimOfVS = -1
  var clusters = -1

  def apply(tCoordinates: List[List[Double]], length: Int, tDimOfVS: Int, tClusters: Int): (List[List[Double]], List[Int]) = {
    coordinates = tCoordinates
    clusters = tClusters
    dimOfVS = tDimOfVS
    centroids = Array.fill(clusters)(Array.fill(dimOfVS)(0.0))
    cluster = Array.fill(length)(-1)
    kmeansPP(length)
    val accuracy = length >> 10
    var changed = 0
    while (changed == 0) {
      var counter: Array[Int] = Array.fill(clusters)(0)
      centroids = Array.fill(clusters)(Array.fill(dimOfVS)(0.0))
      for (i <- List.range(0, length)) {
        counter(cluster(i)) += 1
        for (j <- List.range(0, dimOfVS)) {
          centroids(cluster(i))(j) += coordinates(i)(j)
        }
      }
      for (i <- List.range(0, clusters)) {
        for (j <- List.range(0, dimOfVS)) {
          centroids(i)(j) = centroids(i)(j)/counter(i)
        }
      }
      for (i <- List.range(0, length)) {
        val ind = nearestCentroid(i, clusters)._2
        if (ind != cluster(i)) {
          changed += 1
          cluster(i) = ind
        }
      }
      if (changed <= accuracy) {
        changed = 1
      } else {
        changed = 0
      }
    }
    (centroids.toList.map(_.toList), cluster.toList)
  }
  
  def nearestCentroid(j: Int, i: Int): (Double, Int) = {
    List.range(0, i).foldLeft((100000000000.0, -1)) {
      (acc, k) => {
        val tDist = distance(coordinates(j), centroids(k).toList)
        if(acc._1 > tDist) {
          (tDist, k)
        }
        acc
      }
    }
  }

  def kmeansPP(length: Int): Unit = {
    val randomGenerator = new Random()
    centroids(0) = coordinates(randomGenerator.nextInt(length)).toArray
    var d: Array[Double] = Array.fill(length)(0.0)
    for (i <- List.range(1, clusters)) {
      var sum = 0.0
      for (j <- List.range(0, length)) {
        d(j) = nearestCentroid(j, i)._1
        sum += d(j)
      }
      sum *= randomGenerator.nextDouble()
      var ind = -1
      while (sum > 0) {
        ind += 1
        sum -= d(ind)
      }
      centroids(i) = coordinates(ind).toArray
    }

    for (i <- List.range(0, length)) {
      cluster(i) = nearestCentroid(i, clusters)._2
    }
  }

  def distance(vector1: List[Double], vector2: List[Double]): Double = {
    List.range(0, dimOfVS).foldLeft(0.0) {
      (acc, i) => acc + square(vector1(i) - vector2(i))
    }
  }
  
  def square(x: Double): Double = {
    x * x
  }
}
