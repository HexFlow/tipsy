package tipsy.cluster

import breeze.linalg._, eigSym.EigSym
import breeze.numerics._

object DMtoCM {
  def apply(matrixNetwork: List[List[Double]], length: Int): List[List[Double]] = {
    val gramMatrix: DenseMatrix[Double] = DenseMatrix.zeros[Double](length, length)
    for (i <- 0 to gramMatrix.cols - 1) {
      for (j <- 0 to gramMatrix.rows - 1) {
        gramMatrix(i, j) = (pow((matrixNetwork(i)(0)), 2) + pow((matrixNetwork(j)(0)), 2) - pow(matrixNetwork(i)(j), 2)) / 2
      }
    }
    if (rank(gramMatrix) == 0) {
      println("** [warning] rank of gram matrix was 0")
    }
    val EigSym(eigenValues, eigenVectors) = eigSym(gramMatrix)
    val rootValues: DenseMatrix[Double] = pow(diag(eigenValues), 0.5)
    var flag = 0
    for (i <- 0 to eigenValues.length - 1) {
      if (eigenValues(i) < 0 ) {
        flag = 1
      }
    }
    if (flag == 1) {
      FastMap(matrixNetwork, length, length)._1
    } else {
      val coordinates: DenseMatrix[Double] = eigenVectors * rootValues
      validateCoordinates(coordinates, matrixNetwork)
      val listCoordinates: Array[Array[Double]] = Array.fill(length)(Array.fill(length)(0.0))
      for (i <- 0 to length - 1) {
        for(j <- 0 to length - 1) {
          listCoordinates(i)(j) = coordinates(i, j)
        }
      }
      listCoordinates.map(_.toList).toList
    }
  }

  def validateCoordinates(coordinates: DenseMatrix[Double], matrixNetwork: List[List[Double]]): Unit = {
    for(i <- 0 to coordinates.rows - 1) {
      for (j <- i + 1 to coordinates.rows - 1) {
        var dist: Double = 0.0
        for(k <- 0 to coordinates.cols - 1) {
          dist += pow(coordinates(i, k) - coordinates(j, k), 2)
        }
        dist = pow(dist, 0.5)
        val eps: Double = 0.001
        if (dist > matrixNetwork(i)(j) + eps || dist < matrixNetwork(i)(j) - eps) {
          println("** [warning] coordinates might not be correct")
        }
      }
    }
  }

}
