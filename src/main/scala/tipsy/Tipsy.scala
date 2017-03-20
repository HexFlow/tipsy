package tipsy

import tipsy.frontend._
import scala.collection.mutable.{Set => mSet}

object Tipsy {
  def main(args: Array[String]): Unit = {
    val options = args
      .filter(_.startsWith("-"))
      .map(_.stripPrefix("-"))
      .map(_.stripPrefix("-"))
    val files = args.filter(!_.startsWith("-"))

    var optset: mSet[CLIMode] = mSet()
    var web: Boolean = false
    options.foreach {
      case "pp" => optset add PRINTPARSE
      case "pf" => optset add PRINTFLOW
      case "dp" => optset add DRAWPARSE
      case "df" => optset add DRAWFLOW
      case "le" => optset add LEASTEDIT
      case "web" => web = true
      case unknown => println("[Warning] Ignoring unknown argument: " + unknown)
    }

    // The toList thing is Scala-fu to convert mutable to immutable
    if (web) {
      Web(Set(optset.toList: _*))
    } else {
      CLI(files, Set(optset.toList: _*))
    }
  }
}
