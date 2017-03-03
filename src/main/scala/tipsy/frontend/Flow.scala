package tipsy.frontend

import tipsy.parser._
import tipsy.compiler._
import tipsy.lexer._

import scala.io.Source
import tipsy.compiler.WorkflowCompiler

import scala.util.{Try, Success, Failure}

import dot.render._
import dot.contrib._
import dot.core._
import dot.diagram._
import java.nio.file.Paths

trait FlowDraw {
  implicit def strDrawer: ToRefTree[String] = ToRefTree {
    case x => RefTree.Ref(x, Seq()).rename(x)
  }

  implicit def listDrawer: ToRefTree[List[CFEnum]] = ToRefTree[List[CFEnum]] {
    case x::xs => RefTree.Ref(x, Seq(xs.refTree)).rename(x.flowName)
    case Nil => RefTree.Ref("", Seq()).rename("End")
  }
}

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
