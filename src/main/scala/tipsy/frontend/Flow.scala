package tipsy.frontend

import tipsy.parser._
import tipsy.compiler._
import tipsy.lexer._

import scala.io.Source
import tipsy.compiler.WorkflowCompiler

import scala.util.{Try, Success, Failure}
import java.nio.file.Paths

object FlowGraph {
  def apply(args: Array[String]): Unit = {
    args.map(x => {
      println("Compiling " + x)
      val source = Try(Source.fromFile(x).mkString)

      source match {
        case Success(c) => {
          WorkflowCompiler(c) match {
            case Right(tree) => {
              FlowGraph.compress(tree)
            }
            case Left(err) => {
              println("error")
            }
          }
        }
        case Failure(e) => println("failure")
      }
    }).toList
  }
  def compress(tree: ParseTree): Unit = {
    println(tree.compress)
  }
}
