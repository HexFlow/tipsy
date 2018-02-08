package tipsy.frontend

import scala.concurrent.ExecutionContext

/**
  * CLI: Frontend which parses the CLI configuration and runs services.
  */
class CLI(c: Config) extends TipsyDriver
    with CLIClusterHelpers with CLIExecHelpers {

  implicit val config = c
  implicit val executionContext = ExecutionContext.global

  def run(): Unit = {
    import config._

    if (web) { // WEB actions
      Web(config)
    }

    if (exec) { // EXEC actions
      lazy implicit val validTrees = cliValidTrees

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
