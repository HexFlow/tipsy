package tipsy.frontend

/** @constructor Creates a configuration with all the CLI-configurable switches etc.
  * @param files The files to be used for selected analysis.
  * @param dirs The directories whose files will be used for analysis.
  * @param ids Program IDs (from DB) to run the analysis on.
  * @param limit Maximum number of programs to run the analysis on. Useful when using directories.
  * @param exec Whether to run operations on a given set of input programs.
  * @param parseTree Whether to show ParseTree.
  * @param linearRep Whether to show LinearRepresentation.
  * @param normalRep Whether to show Normalized Linear Representation.
  * @param distance Whether to print distance between programs.
  * @param rawdiff Whether to print raw diffs.
  * @param corrections Whether to print corrections.
  * @param cluster Whether to do cluster related operations.
  * @param ques The question, whose submissions will be used for cluster analysis.
  * @param update Whether to update the cluster for the selected question.
  * @param variance Whether to print the variance of each cluster.
  * @param matrixdump Whether to dump the cluster matrix to disk.
  * @param web Whether the web server will be run.
  * @param host The host address to listen on.
  * @param port The port to listen on.
  * @param admin Whether admin features (program insertion, etc) will be allowed through the web server.
  */
case class Config(
  files: Seq[String] = Seq(),
  dirs: Seq[String] = Seq(),
  ids: Seq[Int] = Seq(),
  limit: Int = -1,

  exec: Boolean = false,
  parseTree: Boolean = false,
  linearRep: Boolean = false,
  normalRep: Boolean = false,

  distance: Boolean = false,
  rawdiff: Boolean = false,
  corrections: Boolean = false,

  cluster: Boolean = false,
  ques: String = "",
  update: Boolean = false,
  variance: Boolean = false,

  matrixdump: Boolean = false,

  web: Boolean = false,
  host: String = "0.0.0.0",
  port: Int = 8070,
  admin: Boolean = false
)
