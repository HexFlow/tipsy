package tipsy.compare

import tipsy.db.schema._
import tipsy.parser._

object ProgStats {
  def apply(code: ParseTree, file: Option[String]): Stats = {

    val counts = code.compress.collect {
      case x @ (IFCOND() | FUNC() | LOOPCOND()) => x
    }.groupBy(identity).mapValues(_.size)

    var depth = 0
    var maxdepth = 0
    val blocks = code.compress.collect {
      case x @ (BLOCKOPEN() | BLOCKCLOSE()) => x
    }.foreach {
      case BLOCKOPEN() =>
        depth = depth + 1
        maxdepth = Integer.max(maxdepth, depth)
      case BLOCKCLOSE() =>
        depth = depth - 1
      case _ => ???
    }

    Stats(
      ifs = Some(counts get IFCOND() getOrElse 0),
      loops = Some(counts get LOOPCOND() getOrElse 0),
      fxns = Some(counts get FUNC() getOrElse 0),
      depth = Some(maxdepth),
      file = file
    )
  }
}
