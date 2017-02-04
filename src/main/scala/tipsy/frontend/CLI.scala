package tipsy.frontend

import scala.io.Source
import tipsy.compiler.WorkflowCompiler

import scala.util.{Try, Success, Failure}


/**
  * CLI: Frontend to handle command line compilations.
  * There may be more frontends later, for instance
  * a web based one.
  */
object CLI {
  def apply(args: Array[String]): Unit = {
    args.map(x => {
      println("Compiling " + x)
      val source = Try(Source.fromFile(x).mkString)

      source match {
        case Success(c) => println(WorkflowCompiler(c))
        case Failure(e) => println(e)
      }
    })
  }
}
