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
    options.foreach {
      case "pp" => optset add PRINTPARSE
      case "pf" => optset add PRINTFLOW
      case "dp" => optset add DRAWPARSE
      case "df" => optset add DRAWFLOW
      case "le" => optset add LEASTEDIT
      case unknown => println("[Warning] Ignoring unknown argument: " + unknown)
    }

    // Kung-fu to convert mutable to immutable
    CLI(files, Set(optset.toList: _*))
  }
}
