package tipsy.compiler

import tipsy.compiler._
import tipsy.compare.ProgStats
import tipsy.db.schema._
import tipsy.frontend._
import tipsy.frontend.Requests._

import java.io._
import scala.sys.process._
import scala.util.Random

object Compiler {
  def apply(prog: String) = {
    getTree(prog)
  }

  def getTree(progcode: String) = {
    val curtime = System.currentTimeMillis().toString()
    val filename =
      "." + curtime + "-" + Random.alphanumeric.take(5).mkString + ".c"

    val w = new BufferedWriter(new FileWriter(filename))
    w.write(progcode)
    w.close()

    val result = WorkflowCompiler(filename)

    s"rm ${filename}".!

    result
  }

  def compileWithStats(
    prog: ProgramInsertReq
  ): Either[CCompilationError, Program] = {
    val idReq: Int = prog.id.getOrElse(0)
    val curtime = System.currentTimeMillis().toString()

    getTree(prog.code).right.flatMap { tree =>
      Right(Program(
        id      = idReq,
        userId  = prog.userId,
        time    = curtime,
        quesId  = prog.quesId,
        code    = prog.code,
        cf      = tree.compress,
        score   = "0",
        correct = false,
        props   = ProgStats(tree)
      ))
    }
  }

  def compileWithStatsProgram(p: Program) = {
    compileWithStats(ProgramInsertReq(
      id     = Some(p.id),
      userId = p.userId,
      quesId = p.quesId,
      code   = p.code
    ))
  }
}
