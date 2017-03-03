package tipsy.frontend

import scala.io.Source
import tipsy.compiler._
import tipsy.compare._

import scala.util.{Try, Success, Failure}


/**
  * CLI: Frontend to handle command line compilations.
  * There may be more frontends later, for instance
  * a web based one.
  */
object CLI {
  def apply(args: Array[String]): Unit = {
    val trees = args.zipWithIndex.map{
      case (file, count) => {
        println(s"[${count+1} of ${args.length}] Compiling " + file)
        Try(Source.fromFile(file).mkString) match {
          case Success(c) => {
            WorkflowCompiler(c) match {
              case Right(tree) => Some(tree)
              case Left(err) => {
                println("Compilation error =>")
                println(err)
                None
              }
            }
          }
          case Failure(e) => {
            println(e)
            None
          }
        }
      }
    }.toList
    LeastEdit(trees.flatMap(x => x))
  }
}
