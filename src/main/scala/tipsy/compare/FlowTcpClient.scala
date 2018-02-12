package tipsy.compare

import com.typesafe.config.ConfigFactory

import scala.sys.process._

object SimpleTcpClient {

  val confFile = ConfigFactory.load()

  val host = confFile.getString("cluster.host")
  val port = confFile.getInt("cluster.port")

  def apply(input: String): String = {
    // Ugly way to get output of clustering.
    val cmd = s"nc -q 0 $host $port"
    val is = new java.io.ByteArrayInputStream(input.getBytes("UTF-8"))
    val out = (cmd #< is).lines_!
    out.mkString("")
  }
}
