package tipsy.frontend

case class Config(
  files: Seq[String] = Seq(),
  dirs: Seq[String] = Seq(),
  ids: Seq[Int] = Seq(),
  limit: Int = -1,

  exec: Boolean = false,
  parseTree: Boolean = false,
  linearRep: Boolean = false,
  distance: Boolean = false,
  corrections: Boolean = false,

  cluster: Boolean = false,
  ques: String = "",
  update: Boolean = false,
  variance: Boolean = false,

  matrixdump: Boolean = false,

  web: Boolean = false,
  host: String = "0.0.0.0",
  port: Int = 8070
)
