package tipsy

import tipsy.frontend._

object Tipsy {
  def main(args: Array[String]): Unit = {
    parser.parse(args, Config()) match {
      case Some(config) => new CLI(config).run()
      case None =>
    }
  }

  val parser = new scopt.OptionParser[Config]("scopt") {
    head("tipsy", "0.1")

    cmd("exec").action( (_, c) => c.copy(exec = true) ).
      text("whether to run operations on a given set of input programs").
      children(
        opt[Seq[String]]("files").valueName("<file1>,<file2>...").action( (x,c) =>
          c.copy(files = x) ).text("files to run analysis on"),

        opt[Seq[Int]]("ids").valueName("<id1>,<id2>...").action( (x,c) =>
          c.copy(ids = x) ).text("program IDs to run analysis on"),

        opt[Unit]('d', "distance").action( (_, c) =>
          c.copy(distance = true) ).text("whether to print distance between programs"),

        opt[Unit]('c', "corrections").action( (_, c) =>
          c.copy(corrections = true) ).text("whether to print corrections"),

        opt[Unit]("parseTree").action( (_, c) =>
          c.copy(parseTree = true) ).text("whether to show ParseTree"),

        opt[Unit]("linearRep").action( (_, c) =>
          c.copy(linearRep = true) ).text("whether to show LinearRepresentation"),

        opt[Unit]("normalRep").action( (_, c) =>
          c.copy(normalRep = true) ).text("whether to show Normalized Linear Representation"),

        cmd("dir").action( (_, c) => c ).
          text("directory to run analysis on").
          children(
            opt[Seq[String]]('n', "names").action( (x, c) =>
              c.copy(dirs= x)),
            opt[Int]('l', "limit").action( (x, c) =>
              c.copy(limit = x) ).text("limit on programs to analyse")
          )
      )

    cmd("cluster").action( (_, c) => c.copy(cluster = true) ).
      text("whether to do cluster related operations").
      children(
        opt[String]('q', "ques").action( (x, c) =>
          c.copy(ques = x) ).text("question whose cluster to run analysis on"),

        opt[Unit]('u', "update").action( (_, c) =>
          c.copy(update = true) ).text("whether to update cluster database"),

        opt[Unit]('v', "variance").action( (_, c) =>
          c.copy(variance = true) ).text("whether to print cluster variance"),

        opt[Unit]("dumpmatrix").abbr("dm").action( (_, c) =>
          c.copy(matrixdump = true) ).text("whether to dump distance matrix")
      )

    cmd("web").action( (_, c) => c.copy(web = true) ).
      text("whether to serve as a web backend").
      children(
        opt[String]("host").action( (x, c) =>
          c.copy(host = x) ).text("address to listen on"),

        opt[Int]("port").action( (x, c) =>
          c.copy(port = x) ).text("port to listen on")
      )

    checkConfig( c =>
      if (c.exec) {
        if (c.files.length == 0 && (c.dirs.length == 0 || c.limit == 0) && c.ids.length == 0)
          failure("no input programs provided to exec mode")
        else success
      } else if (c.cluster) {
          if (c.web || c.exec) failure("cluster mode cannot work with exec or web mode")
          else if (c.ques == "") failure("providing question name is required in cluster mode")
          else success
      } else if (c.web) {
          if (c.cluster || c.exec) failure("web mode cannot work with exec or cluster mode")
          else success
      } else {
        failure("no mode specified. Please specify one mode among exec/cluster/web")
      }
    )
  }

  // def main(args: Array[String]): Unit = {
  //   val files = args.filter(!_.startsWith("-"))

  //   var optset: mMap[CLIMode, String] = mMap()
  //   var web: Boolean = false

  //   var i = 0
  //   var mFiles: Stack[String] = Stack()
  //   while (i < args.length) {
  //     args(i) match {
  //       case "-pp" => optset put (PRINTPARSE, "")
  //       case "-pf" => optset put (PRINTFLOW, "")
  //       case "-le" => optset put (LEASTEDIT, "")
  //       case msg if msg.startsWith("-uc") =>
  //         optset put (UPDATECLUSTER, msg.split("=")(1))
  //       case msg if msg.startsWith("-cv") =>
  //         optset put (CLUSTERVARIANCE, msg.split("=")(1))
  //       case msg if msg.startsWith("-dm=") =>
  //         optset put (DUMPMATRIX, msg.split('=')(1))
  //       case "-cr" => optset put (CORRECTION, "")
  //       case msg if msg.startsWith("-len=") =>
  //         optset put (LIMIT, msg.split('=')(1))
  //       case "-web" => web = true
  //       case msg if msg.startsWith("-") =>
  //         println("[Warning] Ignoring unknown argument: " + msg)
  //       case file => mFiles = mFiles.push(file)
  //     }
  //     i += 1
  //   }

  //   // The toList thing is Scala-fu to convert mutable to immutable
  //   if (web) {
  //     Web(Set())
  //   } else {
  //     CLI(mFiles.toArray, Map(optset.toList: _*))
  //   }
  // }
}
