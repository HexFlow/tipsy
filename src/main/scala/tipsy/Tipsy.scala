package tipsy

import tipsy.frontend._
import scala.collection.mutable.{Map => mMap, Stack}

object Tipsy {
  def main(args: Array[String]): Unit = {
    val files = args.filter(!_.startsWith("-"))

    var optset: mMap[CLIMode, String] = mMap()
    var web: Boolean = false

    var i = 0
    var mFiles: Stack[String] = Stack()
    while (i < args.length) {
      args(i) match {
        case "-pp" => optset put (PRINTPARSE, "")
        case "-pf" => optset put (PRINTFLOW, "")
        case "-le" => optset put (LEASTEDIT, "")
        case msg if msg.startsWith("-dm=") =>
          optset put (DUMPMATRIX, msg.split('=')(1))
        case "-cr" => optset put (CORRECTION, "")
        case msg if msg.startsWith("-len=") =>
          optset put (LIMIT, msg.split('=')(1))
        case "-web" => web = true
        case msg if msg.startsWith("-") =>
          println("[Warning] Ignoring unknown argument: " + msg)
        case file => mFiles = mFiles.push(file)
      }
      i += 1
    }

    // The toList thing is Scala-fu to convert mutable to immutable
    if (web) {
      Web(Set())
    } else {
      CLI(mFiles.toArray, Map(optset.toList: _*))
    }
  }
}
