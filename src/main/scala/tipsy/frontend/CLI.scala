package tipsy.frontend

import scala.concurrent.ExecutionContext

/** Frontend which parses the CLI configuration and runs services.
  * @param config Configuration satisfying [[tipsy.frontend.Config]].
  */
class CLI(implicit val config: Config) extends TipsyDriver
    with CLIClusterHelpers with CLIExecHelpers {

  implicit val executionContext = ExecutionContext.global

  /** Runs services according to the input configuration.
    */
  def run(): Unit = {
    import config._

    if (web) { // WEB actions
      Web(config)
    }

    if (exec) { // EXEC actions
      implicit val validTrees = cliValidTrees

      if (distance) cliDistance
      if (corrections) cliCorrections
    }

    if (cluster) { // CLUSTER actions
      implicit val quesId = ques

      if (update) cliUpdateClusters
      if (variance) cliVariance
      if (matrixdump) cliMatrixDump
    }

    driver.close()
  }
}
