package gala.frontend

import scala.io.Source
import gala.compiler._

/**
  * CLI: Frontend to handle command line compilations.
  * There may be more frontends later, for instance
  * a web based one.
  */
object CLI {
  def apply(args: Array[String]) {
    args.map(x => {
      println("Compiling " + x)
      val source = Source.fromFile(x)
      val code =
        try {
          WorkflowCompiler(source.mkString)
        } catch {
          case _: Throwable => {
            println("Error while reading file: " + x)
          }
        } finally {
          source.close()
        }
    })
  }
}
