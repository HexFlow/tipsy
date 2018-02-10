package tipsy.frontend

import tipsy.compare._
import tipsy.compiler.WorkflowCompiler
import tipsy.parser.ParseTree

import java.io.File

import scala.concurrent.ExecutionContext

import scalaz._, Scalaz._

trait CLIExecHelpers extends TipsyDriver {
  implicit val config: Config
  implicit val executionContext: ExecutionContext

  import config._

  def cliDistance(implicit validTrees: Vector[(ParseTree, String)]) = {
    validTrees.map(x => NormalizeParseTree(x._1)).sequenceU match {
      case Left(err) => println("ERROR: Could not normalize some trees: " ++ err.toString)
      case Right(cfList) =>
        val pairs = cfList.zipWithIndex.combinations(2).map {
          case Seq((cf1, idx1), (cf2, idx2)) =>
            val force = 100/(0.1+Compare.findDist(cf1, cf2).dist)
            (idx1, idx2, force)
        }.toList
        DistanceDraw(pairs, cfList.length, validTrees.map(_._2).toList)
    }
  }

  def cliCorrections(implicit validTrees: Vector[(ParseTree, String)]) = {
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

  def cliValidTrees(): Vector[(ParseTree, String)] = {
    val dirFileNames = dirs.map(expandDir).flatten
    val dirFiles = if (limit == -1) dirFileNames else dirFileNames.take(limit)

    val finalFiles = dirFiles ++ files

    finalFiles.zipWithIndex.map{
      case (file, count) => {
        println(s"[${count+1} of ${files.length}] Compiling " + file)
        WorkflowCompiler(file) match {
          case Right(tree) => {
            if (parseTree) println(tree)
            if (linearRep) println(tree.compress)
            if (normalRep) {
              NormalizeParseTree(tree) match {
                case Left(err) => println("There was an error in normalization: " ++ err.toString)
                case Right(NormCode(nfxns)) =>
                  nfxns.map {
                    case NormFxn(name, cfs) =>
                      println("Function: " ++ name)
                      cfs.map { cf =>
                        println(cf.line.toString + ":" + cf.column.toString + "\t" + cf.toString)
                      }
                  }
              }
            }
            (Some(tree), file)
          }
          case Left(err) => {
            println("Compilation error =>")
            println(err)
            (None, file)
          }
        }
      }
    }.collect { case (Some(x), y) => (x, y) }.toVector
  }

  private def expandDir(name: String): List[String] = {
    val fl = new File(name)
    if (fl.isDirectory()) {
      fl.listFiles.filter(_.isFile).toList.map(x => x.getPath()).filter(_.endsWith(".c"))
    } else {
      List(name)
    }
  }

}
