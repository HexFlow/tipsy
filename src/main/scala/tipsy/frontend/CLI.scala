package tipsy.frontend

import tipsy.compiler._
import tipsy.compare._
import tipsy.db.TipsyPostgresProfile.api._
import tipsy.db.schema._

import java.io.File
import java.io.PrintWriter

import scala.concurrent.{ Future, Await }
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext


import scalaz._, Scalaz._

sealed trait CLIMode
case object LEASTEDIT extends CLIMode
case object LIMIT extends CLIMode
case object PRINTPARSE extends CLIMode
case object PRINTFLOW extends CLIMode
case object CORRECTION extends CLIMode
case object DUMPMATRIX extends CLIMode
case object UPDATECLUSTER extends CLIMode
case object CLUSTERVARIANCE extends CLIMode

/**
  * CLI: Frontend to handle command line compilations.
  * There may be more frontends later, for instance
  * a web based one.
  */
object CLI extends TipsyDriver with ClusterActions {
  implicit val executionContext = ExecutionContext.global

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
          distTable.filter(_.quesId === quesId).map(e => (e.id1, e.id2, e.dist)).result
        }
        matrixStr = Dists.getAsJson(matrix)
        writer = new PrintWriter(new File(s"matrix_${quesId}"))
        _ <- Future(writer.write(matrixStr))
        _ <- Future(writer.close())
        _ <- Future(println("Finished task"))
      } yield ()
      Await.result(action, Duration.Inf)
    }

    if (modes contains UPDATECLUSTER) {
      val its = 1
      val st = System.currentTimeMillis()
      1 to its foreach { _ =>
        val action = for {
          matrix <- driver.runDB {
            distTable.filter(_.quesId === modes(UPDATECLUSTER)).map(e => (e.id1, e.id2, e.dist)).result
          }
          _ <- doUpdateClusters(matrix, modes(UPDATECLUSTER))
        } yield ()
        Await.result(action, Duration.Inf)
      }
      val total = System.currentTimeMillis() - st
      println("Time needed per clustering call: " ++ (total.toDouble/its).toString)
    }

    if (modes contains CLUSTERVARIANCE) {
      val quesId = modes(CLUSTERVARIANCE)
      val action = for {
        clusters <- driver.runDB {
          clusterTable.filter(_.quesId === quesId).map(_.cluster).result
        }.map(_.headOption.getOrElse(throw new Exception(s"Cluster for ${quesId} not found in database.")))

        _ <- Future(println("SINGLETON CLUSTER COUNT: " ++ clusters.filter(_.length == 1).length.toString))
        _ <- Future(println("CLUSTER COUNT: " ++ clusters.length.toString))

        varsAndLen <- Future.sequence(
          clusters.map { cluster =>
            if (cluster.length > 1) {
              for {
                progs <- Future.sequence(cluster.map(progId =>
                  driver.runDB {
                    progTable.filter(_.id === progId).result
                  }.map(_.headOption.getOrElse(throw new Exception(s"Could not find program ${progId}")))
                ))
                scores = progs.map(_.score.toDouble)
                variance = getVariance(scores)
                _ <- Future(println(s"${scores.length.toString} -> ${variance.toString}"))
                // _ <- Future(println(scores.length.toString ++ " -> " ++ variance.toString ++
                //   "\t --> " ++ progs.map(_.props.file.getOrElse("")).mkString(",") ++
                //   "\t --> " ++ progs.map(_.id).mkString(",")))
              } yield (variance, scores.length, scores)
            } else Future(0.0, 0, List())
          }
        )

        _ <- Future(println("Weighted average of non-singleton variance: " ++
          (varsAndLen.map(x => x._1 * x._2).sum / varsAndLen.map(_._2).sum).toString
        ))
        _ <- Future(println("Non-singleton overall: " ++
          getVariance(varsAndLen.flatMap(_._3)).toString
        ))
        _ <- Future(println("Non-singleton count: " ++
          varsAndLen.map(_._2).sum.toString
        ))
      } yield ()
      Await.result(action, Duration.Inf)
      println("Overall: " ++ Await.result(findVarianceOfQues(quesId), Duration.Inf).toString)
    }

    if (modes contains LEASTEDIT) {
      validTrees.map(x => NormalizeParseTree(x._1)).sequenceU match {
        case Left(err) => println("ERROR: Could not normalize some trees: " ++ err.toString)
        case Right(cfList) =>
          val pairs = cfList.zipWithIndex.combinations(2).map {
            case Seq((cf1, idx1), (cf2, idx2)) =>
              val force = 100/(0.1+Compare.findDist(cf1, cf2).dist)
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

    driver.close()
  }
}
