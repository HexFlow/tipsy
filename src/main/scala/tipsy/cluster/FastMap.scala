package tipsy.cluster

import scala.util.Random

object FastMap {

  var projectionsInK = Array[Array[Double]]()
  var pivots = Array[(Int, Int)]()
  var cols = 0
  val pivotIterations = 50

  def apply (matrixNetwork: List[List[Double]], length: Int, dimOfVS: Int): (List[List[Double]], List[(Int, Int)]) = {
    projectionsInK = Array.fill(length)(Array.fill(dimOfVS)(0.0))
    pivots = Array.fill(dimOfVS)((-1, -1))
    cols = 0
    generate(matrixNetwork, length, dimOfVS)
    validateProjections(matrixNetwork, length, dimOfVS)
    (projectionsInK.map(_.toList).toList, pivots.toList)
  }

  def validateProjections (matrixNetwork: List[List[Double]], length: Int, dimOfVS: Int): Unit = {
    for(i <- 0 to length - 1) {
      for(j <- i + 1 to length - 1) {
        print(s"($i, $j): ${matrixNetwork(i)(j)} ")
        val dist = List.range(0, dimOfVS).foldLeft(0.0) {
          (acc, cur) => acc + square(projectionsInK(i)(cur) - projectionsInK(j)(cur))
        }
        println(0.0 - matrixNetwork(i)(j) + math.sqrt(dist))
        if (math.sqrt(dist) > matrixNetwork(i)(j) + 10 || math.sqrt(dist) < matrixNetwork(i)(j) - 10) {
          println("** [warning] high difference")
        }
      }
    }
  }

  def generate (matrixNetwork: List[List[Double]], length: Int, dimOfVS: Int): Unit = {
    if (dimOfVS == 0) {
      return
    }

    val currPivots = pickPivots(matrixNetwork, length)
    println(s"Picked pivots as $currPivots at dim = $cols")
    if (distance(matrixNetwork, currPivots) == 0) {
      return
    }

    for (i <- List.range(0, length)) {
      projectionsInK(i)(cols) = findProjection(matrixNetwork, currPivots, i)
    }

    cols += 1
    generate(matrixNetwork, length, dimOfVS - 1)
  }

  def findProjection(matrixNetwork: List[List[Double]], nodes: (Int, Int), i: Int): Double = {
    val din1 = distance(matrixNetwork, (i, nodes._1))
    val din2 = distance(matrixNetwork, (i, nodes._2))
    val dn1n2 = distance(matrixNetwork, nodes)
    (din1 + dn1n2 - din2) / (2 * math.sqrt(dn1n2))
  }

  def distance(matrixNetwork: List[List[Double]], nodes: (Int, Int)): Double = {
    List.range(0, cols).foldLeft(square(matrixNetwork(nodes._1)(nodes._2))) {
      (acc, cur) => acc - square(projectionsInK(nodes._1)(cur) - projectionsInK(nodes._2)(cur))
    }
  }

  def square(x: Double): Double = {
    x * x
  }

  def pickPivots (matrixNetwork: List[List[Double]], length: Int): (Int, Int) = {
    val randomGenerator = new Random()
    var p1 = randomGenerator.nextInt(length)
    var p2 = -1

    var it = pivotIterations
    var tp = -1

    while (it > 0) {
      tp = furthestNode(matrixNetwork, length, p1)
      if (tp == p2) {
        return assignPivots(p1, p2)
      }
      p2 = tp
      tp = furthestNode(matrixNetwork, length, p2)
      if (tp == p1) {
        return assignPivots(p1, p2)
      }
      p1 = tp
      it -= 1
    }

    assignPivots(p1, p2)
  }

  def assignPivots (p1: Int, p2: Int): (Int, Int) = {
    pivots(cols) = (p1, p2)
    pivots(cols)
  }

  def furthestNode(matrixNetwork: List[List[Double]], length: Int, node: Int): Int = {
    var d = -1000000007.0
    var ind = -1
    for (i <- List.range(0, length)) {
      var tempD = distance(matrixNetwork, (i, node))
      if (tempD > d) {
        d = tempD
        ind = i
      }
    }
    ind
  }
}
