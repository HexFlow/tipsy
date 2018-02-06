package tipsy.db.schema

import tipsy.db.Constraints._
import tipsy.db.TipsyPostgresProfile.api._
import tipsy.compare._

case class Program (
  id: Int,
  userId: String,
  time: String,
  quesId: String,
  code: String,
  cf: NormCode,
  score: String,
  correct: Boolean,
  props: Stats
)

class Programs(tag: Tag) extends
    Table[Program](tag, "PROGRAMS") with WithPrimaryKey {

  def id: Rep[Int] = column[Int]("SUB_ID", O.PrimaryKey, O.AutoInc)
  def userId: Rep[String] = column[String]("USER_ID")
  def time: Rep[String] = column[String]("SUB_TIME")
  def quesId: Rep[String] = column[String]("QUES_ID")
  def code: Rep[String] = column[String]("CODE")
  def cf: Rep[NormCode] = column[NormCode]("CF")
  def score: Rep[String] = column[String]("SCORE")
  def correct: Rep[Boolean] = column[Boolean]("CORRECT")
  def props: Rep[Stats] = column[Stats]("PROPS")

  def * = (
    (id, userId, time, quesId, code, cf, score, correct, props) <>
      ((Program.apply _).tupled, Program.unapply)
  )
}
