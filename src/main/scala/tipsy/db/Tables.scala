package tipsy.db

import slick.driver.PostgresDriver.api._
import slick.lifted.{ProvenShape, ForeignKeyQuery}

case class Program(
  userId: String,
  quesId: String,
  code: String
)

class Programs(tag: Tag)
    extends Table[(Int, String, String, String, String, String)](tag, "PROGRAMS") {

  type Fields = (Int, String, String, String, String, String)

  def id: Rep[Int] = column[Int]("SUB_ID", O.PrimaryKey, O.AutoInc)
  def userId: Rep[String] = column[String]("USER_ID")
  def time: Rep[String] = column[String]("SUB_TIME")
  def quesId: Rep[String] = column[String]("QUES_ID")
  def code: Rep[String] = column[String]("CODE")
  def score: Rep[String] = column[String]("SCORE")


  def * : ProvenShape[Fields] =
    (id, userId, time, quesId, code, score)
}
