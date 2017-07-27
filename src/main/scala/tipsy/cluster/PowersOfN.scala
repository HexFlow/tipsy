package tipsy.compare

object PowersOfN {
  def apply(n: Double, length: Int): Array[Double] = {
    var ret: Array[Double] = Array.fill(length + 1)(1.0)
    for (i <- 1 to length) {
      ret(i) = ret(i-1)*n
    }
    ret
  }
}
