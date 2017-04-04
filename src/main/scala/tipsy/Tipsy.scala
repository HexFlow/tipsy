package tipsy

import tipsy.frontend._
import scala.collection.mutable.{Map => mMap}

object Tipsy {
  def main(args: Array[String]): Unit = {
    val options = args
      .filter(_.startsWith("-"))
      .map(_.stripPrefix("-"))
      .map(_.stripPrefix("-"))
    val files = args.filter(!_.startsWith("-"))

    var optset: mMap[CLIMode, String] = mMap()
    var web: Boolean = false

    options.foreach {
      case "pp" => optset put (PRINTPARSE, "")
      case "pf" => optset put (PRINTFLOW, "")
      case "dp" => optset put (DRAWPARSE, "")
      case "df" => optset put (DRAWFLOW, "")
      case "le" => optset put (LEASTEDIT, "")
      case msg if msg.startsWith("len") => optset put (LEASTEDITLIMIT, msg.split('=')(1))
      case "web" => web = true
      case unknown => println("[Warning] Ignoring unknown argument: " + unknown)
    }

    // The toList thing is Scala-fu to convert mutable to immutable
    if (web) {
      Web(Set())
    } else {
      CLI(files, Map(optset.toList: _*))
    }
  }
}
