package tipsy.db.schema

case class Stats (
  ifs: Option[Int],
  loops: Option[Int],
  fxns: Option[Int],
  depth: Option[Int],
  file: Option[String]
)
