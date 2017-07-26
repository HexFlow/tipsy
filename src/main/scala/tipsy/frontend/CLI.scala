package tipsy.frontend

import scala.io.Source
import tipsy.compiler._
import tipsy.compare._
import tipsy.parser._
import tipsy.cluster._

import reftree.render._
import reftree.contrib._
import reftree.diagram._
import reftree.core._
import reftree.contrib.SimplifiedInstances.list

import java.nio.file.Paths
import java.io.File

import scala.util.{Try, Success, Failure}

sealed trait CLIMode
case object LEASTEDIT extends CLIMode
case object LEASTEDITLIMIT extends CLIMode
case object DRAWPARSE extends CLIMode
case object PRINTPARSE extends CLIMode
case object DRAWFLOW extends CLIMode
case object PRINTFLOW extends CLIMode
case object CLUSTER extends CLIMode
case object EQUALCLUSTER extends CLIMode

trait FlowDraw {
  implicit def cfListDrawer: ToRefTree[List[CFEnum]] = ToRefTree[List[CFEnum]] {
    case x::xs => RefTree.Ref(x, xs.map(_.refTree)).rename(x.flowName)
    case Nil => RefTree.Ref("", Seq()).rename("End")
  }

  implicit def cfDrawer: ToRefTree[CFEnum] = ToRefTree[CFEnum] {
    case x => RefTree.Ref(x, Seq()).rename(x.flowName)
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

  def expandDir(name: String): List[String] = {
    val fl = new File(name)
    if (fl.isDirectory()) {
      fl.listFiles.filter(_.isFile).toList.map(x => x.getPath()).filter(_.endsWith(".c"))
    } else {
      List(name)
    }
  }

  def apply(filesOrig: Array[String], modes: Map[CLIMode, String]): Unit = {
    val files =
      if (modes contains LEASTEDITLIMIT) {
        println("Value is " + Integer.parseInt(modes(LEASTEDITLIMIT)))
        filesOrig.map(expandDir).flatten.take(Integer.parseInt(modes(LEASTEDITLIMIT)))
      } else {
        filesOrig.map(expandDir).flatten
      }

    val trees = files.zipWithIndex.map{
      case (file, count) => {
        println(s"[${count+1} of ${files.length}] Compiling " + file)
        WorkflowCompiler(file) match {
          case Right(tree) => {
            if (modes contains PRINTPARSE) println(Right(tree))
            if (modes contains PRINTFLOW) println(FlowGraphTweaks(tree.compress))
            if (modes contains DRAWPARSE)
              renderer.render(s"parsetree-$count", Diagram(tree))
            if (modes contains DRAWFLOW)
              renderer.render(s"flowgraph-$count", Diagram(tree.compress))
            (Some(tree), file)
          }
          case Left(err) => {
            println("Compilation error =>")
            println(err)
            (None, file)
          }
        }
      }
    }.toList
    if (modes contains LEASTEDIT) {
      val validTrees = trees.collect { case (Some(x), y) => (x, y) }
      if (modes contains CLUSTER) {
        lazy val leastEdited = LeastEdit(validTrees.map(_._1), true)
        val len = validTrees.length
        val matrixNetwork: Array[Array[Double]] = Array.fill(len)(Array.fill(len)(0.0))
        for (i <- leastEdited) {
          matrixNetwork(i._1)(i._2) = i._3
          matrixNetwork(i._2)(i._1) = i._3
        }
        val equalSized = if (modes contains EQUALCLUSTER) true else false
        Clusterify(matrixNetwork.map(_.toList).toList, len, validTrees.map(_._2), Integer.parseInt(modes(CLUSTER)), equalSized)
      }
      else {
        DistanceDraw(LeastEdit(validTrees.map(_._1), false), validTrees.length, validTrees.map(_._2))
      }
    }
  }
}
