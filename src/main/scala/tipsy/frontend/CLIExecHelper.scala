package tipsy.frontend

import tipsy.compare._
import tipsy.compiler.WorkflowCompiler
import tipsy.parser.ParseTree
import Types.EditRet

import java.io.File

import scalaz._, Scalaz._
import io.circe.syntax._

/** Contains handlers for the simple execute commands which
  * can be run via CLI.
  */
trait CLIExecHelpers extends TipsyDriver with JsonSupport {

  /** The configuration provided via CLI arguments, containing commands
    * and programs to be run.
    */
  implicit val config: Config

  import config._

  /** Shows distances between the input program parse trees.
    * @param validTrees Input instances of Vector[CompileResult].
    */
  def cliDistance(implicit validTrees: Vector[CompileResult]) = {
    validTrees.map(x => NormalizeParseTree(x.tree)).sequenceU match {
      case -\/(err) => println("ERROR: Could not normalize some trees: " ++ err.toString)
      case \/-(cfList) =>
        val pairs = cfList.zipWithIndex.combinations(2).map {
          case Seq((cf1, idx1), (cf2, idx2)) =>
            val force = 100/(0.1+Compare.findDist(cf1, cf2).dist)
            (idx1, idx2, force)
        }.toList
        DistanceDraw(pairs, cfList.length, validTrees.map(_.file).toList)
    }
  }

  /** Shows the raw diff between the provided list of parse trees.
    * @param validTrees Input instances of Vector[CompileResult],
    * where the string is the name of the program.
    */
  def cliRawDiff(implicit validTrees: Vector[CompileResult]) = {
    if (validTrees.length == 2) {
      val res = for {
        cf1 <- NormalizeParseTree(validTrees(0).tree)
        cf2 <- NormalizeParseTree(validTrees(1).tree)
      } yield Compare.findDist(cf1, cf2)

      res match {
        case -\/(err) => println("Error while fetching corrections: " ++ err.toString)
        case \/-(EditRet(diffs, dist)) =>
          println("Edit ret:")
          println("Distance: " ++ dist.toString)
          println("Diffs:")
          diffs.map(diff => println(diff.asJson))
      }
    } else {
      println("Corrections are not provided when given more than two files.")
    }
  }

  /** Shows the corrections between the provided list of parse trees.
    * @param validTrees Input instances of Vector[CompileResult],
    * where the string is the name of the program.
    */
  def cliCorrections(implicit validTrees: Vector[CompileResult]) = {
    if (validTrees.length == 2) {
      val res = for {
        cf1 <- NormalizeParseTree(validTrees(0).tree)
        cf2 <- NormalizeParseTree(validTrees(1).tree)
        EditRet(diffs, dist) = Compare.findDist(cf1, cf2)
      } yield ()

      res match {
        case -\/(err) => println("Error while fetching corrections: " ++ err.toString)
        case \/-(_) => println("Corrections has not been implemented yet. Use --rawdiff to see the raw diffs instead.")
      }
    } else {
      println("Corrections are not provided when given more than two files.")
    }
  }

  /** Creates instances of Vector[CompileResult] (containing parse trees and codes)
    * using the instanciated member config. Reads the programs to be compiled from
    * it, and compiles them to a pair of parse trees and the name of the program.
    */
  def cliValidTrees(): Vector[CompileResult] = {
    val dirFileNames = dirs.map(expandDir).flatten
    val dirFiles = if (limit == -1) dirFileNames else dirFileNames.take(limit)

    val finalFiles = dirFiles ++ files

    finalFiles.zipWithIndex.map{
      case (file, idx) => {
        println(s"[${idx+1} of ${finalFiles.length}] Compiling " + file)

        (for {
          code <- WorkflowCompiler.getCode(file)
          tree <- WorkflowCompiler.getTree(code)
          normalized <- NormalizeParseTree(tree)
          NormCode(nfxns) = normalized
        } yield {
          if (parseTree) println(tree)
          if (linearRep) println(tree.compress)
          if (normalRep) {
            nfxns.map {
              case NormFxn(name, cfs) =>
                println("Function: " ++ name)
                cfs.map { cf =>
                  println(cf.position + "\t" + cf.toString)
                }
            }
          }
          CompileResult(tree, code, file)
        }).leftMap { err =>
          println(s"Error while compiling ${file}: " + err)
        }
      }
    }.collect { case \/-(x) => x }.toVector
  }

  case class CompileResult(
    tree: ParseTree,
    file: String,
    code: String
  )

  private def expandDir(name: String): List[String] = {
    val fl = new File(name)
    if (fl.isDirectory()) {
      fl.listFiles.filter(_.isFile).toList.map(x => x.getPath()).filter(_.endsWith(".c"))
    } else {
      List(name)
    }
  }

}
