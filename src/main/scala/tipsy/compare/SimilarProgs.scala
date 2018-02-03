package tipsy.compare

import tipsy.db._
import tipsy.db.schema._
import tipsy.db.TipsyPostgresProfile.api._

import tipsy.frontend._
import tipsy.compare._

object SimilarProgs extends Ops with TipsyDriver {

  def apply(p: Program) = {
      val props = p.props

      val similarProgs: List[Program] = driver.runDB {
        progTable.filter { row =>
          row.quesId === p.quesId
          row.id =!= p.id
          // row.correct === true &&
          // row.userId =!= p.userId
        }.result
      }.toList

      similarProgs.filter { elem =>
        try {
          val stats = elem.props
          val sat = for {
            f1 <- stats.fxns
            f2 <- props.fxns
            r1 <- Some(math.abs(f1 - f2))
            if (r1 <= 1)

            i1 <- stats.ifs
            i2 <- props.ifs
            r2 <- Some(math.abs(i1 - i2))
            if (r2 <= 1)

            l1 <- stats.loops
            l2 <- stats.loops
            r3 <- Some(math.abs(l1 - l2))
            if (r3 <= 1)

          } yield ()

          sat match {
            case Some(_) => true
            case _ => false
          }
        } catch {
          case e: Throwable => false
        }
      }
  }
}
