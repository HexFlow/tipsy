package tipsy.frontend

import scala.io.Source
import tipsy.compiler._
import tipsy.compare._
import tipsy.parser._
import tipsy.cluster._
import tipsy.db.TipsyPostgresProfile.api._
import tipsy.db.schema._

import java.nio.file.Paths
import java.io.File
import java.io.PrintWriter

import scala.concurrent.{ Future, Await }
import scala.concurrent.duration.Duration

import scala.util.{Try, Success, Failure}
import scalaz._, Scalaz._

sealed trait CLIMode
case object LEASTEDIT extends CLIMode
case object LIMIT extends CLIMode
case object PRINTPARSE extends CLIMode
case object PRINTFLOW extends CLIMode
case object CORRECTION extends CLIMode
case object DUMPMATRIX extends CLIMode

/**
  * CLI: Frontend to handle command line compilations.
  * There may be more frontends later, for instance
  * a web based one.
  */
object CLI extends TipsyDriverWithoutActors {

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
      if (modes contains LIMIT) {
        println("Value is " + Integer.parseInt(modes(LIMIT)))
        filesOrig.map(expandDir).flatten.take(Integer.parseInt(modes(LIMIT)))
      } else {
        filesOrig.map(expandDir).flatten
      }

    val trees = files.zipWithIndex.map{
      case (file, count) => {
        println(s"[${count+1} of ${files.length}] Compiling " + file)
        WorkflowCompiler(file) match {
          case Right(tree) => {
            if (modes contains PRINTPARSE) println(Right(tree))
            if (modes contains PRINTFLOW) println(tree.compress)
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
    lazy val validTrees = trees.collect { case (Some(x), y) => (x, y) }

    if (modes contains DUMPMATRIX) {
      val quesId = modes(DUMPMATRIX)
      val action = for {
        matrix <- driver.runDB {
          distTable.filter(_.quesId === quesId).map(e => (e.id, e.dists)).result
        }
        matrixStr = Dists.getAsDump(matrix)
        writer = new PrintWriter(new File(s"matrix_${quesId}"))
        _ <- Future(writer.write(matrixStr))
        _ <- Future(writer.close())
        _ <- Future(println("Finished task"))
      } yield ()
      Await.result(action, Duration.Inf)
    }

    if (modes contains LEASTEDIT) {
      validTrees.map(x => NormalizeParseTree(x._1)).sequenceU match {
        case Left(err) => println("ERROR: Could not normalize some trees: " ++ err.toString)
        case Right(cfList) =>
          val pairs = cfList.zipWithIndex.combinations(2).map {
            case Seq((cf1, idx1), (cf2, idx2)) =>
              val force = 1.0/(0.1+Compare.findDist(cf1, cf2).dist)
              (idx1, idx2, force)
          }.toList
          DistanceDraw(pairs, cfList.length, validTrees.map(_._2))
      }
    } else if (modes contains CORRECTION) {
      if (validTrees.length == 2) {
        val res = for {
          cf1 <- NormalizeParseTree(validTrees(0)._1).right
          cf2 <- NormalizeParseTree(validTrees(1)._1).right
        } yield Compare.findDist(cf1, cf2)

        res match {
          case Left(err) => println("Error while fetching corrections: " ++ err.toString)
          case Right(EditRet(diffs, dist)) =>
            println("Edit ret:")
            println("Distance: " ++ dist.toString)
            println("Diffs:")
            diffs.map {
              case Diff(change, addEntry, delEntry, fxn) =>
                println(change.string ++ ": " ++ addEntry.toString ++ " ====>> " ++
                  delEntry.toString ++ " in " ++ fxn)
            }
        }
      }
    }
  }
}
