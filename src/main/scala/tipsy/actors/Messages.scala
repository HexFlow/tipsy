package tipsy.actors

import tipsy.parser._
import tipsy.db._
import tipsy.db.schema._
import tipsy.frontend.Requests._

object Messages {
  case class CompileWithStats(prog: ProgramInsertReq)
  case class CompileAndGetTree(prog: Program)
  case class CompileAndGetTrees(progs: List[Program])
  case class CompileAndGetTreesResp(progs: List[ParseTree])

  case class SimilarCheck(id: Int)
  case class SimilarCheckResp(progs: List[Program])

  case class InsertReq(prog: Program)
  case class InsertResp(id: Int)
}
