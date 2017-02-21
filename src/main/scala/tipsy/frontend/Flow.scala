package tipsy.frontend

import tipsy.parser._
import tipsy.compiler._
import tipsy.lexer._

import scala.io.Source
import tipsy.compiler.WorkflowCompiler

import scala.util.{Try, Success, Failure}

import dot.render._
import dot.contrib._
import dot.diagram._
import java.nio.file.Paths

object FlowGraph extends FlowDraw {
  val renderer = Renderer(
    renderingOptions = RenderingOptions(density = 75),
    directory = Paths.get("."),
    format = "ps"
  )

  def apply(args: Array[String]): Unit = {
    args.map(x => {
      println("Compiling " + x)
      val source = Try(Source.fromFile(x).mkString)

      source match {
        case Success(c) => {
          WorkflowCompiler(c) match {
            case Right(tree) => {
              println(tree.compress)
              renderer.render("flowgraph", Diagram(tree.compress))
            }
            case Left(err) => {
              println("Error creating flowgraph")
              println(err)
            }
          }
        }
        case Failure(e) => {
          println("Error while trying to compile")
          println(e)
        }
      }
    }).toList
  }
}
