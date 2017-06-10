package tipsy.cluster

object Clusterify {
  def apply(matrixNetwork: List[List[Double]], length: Int, names: List[String], cluster: Double): Unit = {
    println(matrixNetwork)
    val clusters  =  10 // Will be made as argument parser later
    lazy val fastmapResult = FastMap(matrixNetwork, length, clusters)
    println(fastmapResult._1)
  }
}
