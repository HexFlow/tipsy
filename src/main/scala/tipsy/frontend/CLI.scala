package tipsy.frontend

import scala.concurrent.ExecutionContext

/** @constructor Frontend which parses the CLI configuration and runs services.
  * @param cfg Configuration satisfying [[tipsy.frontend.Config]].
  */
class CLI(cfg: Config) extends TipsyDriver
    with CLIClusterHelpers with CLIExecHelpers {

  implicit val config = cfg
  implicit val executionContext = ExecutionContext.global

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
