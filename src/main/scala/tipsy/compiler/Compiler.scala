package tipsy.compiler

import scalaz._
import tipsy.compare.{ProgStats, NormalizeParseTree}
import tipsy.db.schema._
import tipsy.db.Requests._
import scala.sys.process._
import java.io._
import scala.util.Random

object Compiler {
  def getFilename(): String = {
    val curtime = System.currentTimeMillis().toString()
    "." + curtime + "-" + Random.alphanumeric.take(5).mkString + ".c"
  }

  def apply(prog: String) = {
    getTree(prog)
  }

  def getTree(code: String) = {
    val filename = getFilename()

    val w = new BufferedWriter(new FileWriter(filename))
    w.write(code)
    w.close()

    val result = WorkflowCompiler(filename)
    s"rm -f ${filename}".!
    result
  }

  def compileWithStats(
    prog: ProgramInsertReq
  ): \/[CCompilationError, Program] = {
    val idReq: Int = prog.id.getOrElse(0)
    val curtime = System.currentTimeMillis().toString()

    for {
      tree <- getTree(prog.code)
      cf <- NormalizeParseTree(tree)
    } yield Program(
        id      = idReq,
        userId  = prog.userId,
        time    = curtime,
        quesId  = prog.quesId,
        code    = prog.code,
        cf      = cf,
        score   = prog.score.getOrElse("0"),
        correct = false,
        props   = ProgStats(tree, prog.file)
      )
  }
}
