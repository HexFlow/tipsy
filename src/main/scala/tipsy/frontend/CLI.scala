package tipsy.frontend

import scala.io.Source
import tipsy.compiler._
import tipsy.compare._
import tipsy.parser._

import dot.render._
import dot.contrib._
import dot.diagram._
import dot.core._
import java.nio.file.Paths

import scala.util.{Try, Success, Failure}

sealed trait CLIMode
case object LEASTEDIT extends CLIMode
case object DRAWPARSE extends CLIMode
case object PRINTPARSE extends CLIMode
case object DRAWFLOW extends CLIMode
case object PRINTFLOW extends CLIMode

trait FlowDraw {
  implicit def cfListDrawer: ToRefTree[List[CFEnum]] = ToRefTree[List[CFEnum]] {
    case x::xs => RefTree.Ref(x, Seq(xs.refTree)).rename(x.flowName)
    case Nil => RefTree.Ref("", Seq()).rename("End")
  }
}

/**
  * CLI: Frontend to handle command line compilations.
  * There may be more frontends later, for instance
  * a web based one.
  */
object CLI extends TreeDraw with FlowDraw {
  val renderer = Renderer(
    renderingOptions = RenderingOptions(density = 75),
    directory = Paths.get("."),
    format = "ps"
  )

  def apply(files: Array[String], modes: Set[CLIMode]): Unit = {
    val trees = files.zipWithIndex.map{
      case (file, count) => {
        println(s"[${count+1} of ${files.length}] Compiling " + file)
        Try(Source.fromFile(file).mkString) match {
          case Success(c) => {
            WorkflowCompiler(c) match {
              case Right(tree) => {
                if (modes contains PRINTPARSE) println(tree)
                if (modes contains PRINTFLOW) println(tree.compress)
                if (modes contains DRAWPARSE)
                  renderer.render(s"parsetree-$count", Diagram(tree))
                if (modes contains DRAWFLOW)
                  renderer.render(s"flowgraph-$count", Diagram(tree.compress))
                Some(tree)
              }
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
    if (modes contains LEASTEDIT) {
      val validTrees = trees.flatMap(x => x)
      DistanceDraw(LeastEdit(validTrees), validTrees.length)
    }
  }
}
