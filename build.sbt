name := "tipsy"

version := "0.1"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  Resolver.bintrayRepo("stanch", "maven"),
  Resolver.bintrayRepo("drdozer", "maven")
)

libraryDependencies ++= Seq(
  "org.scalatest"           %% "scalatest"                % "2.2.6" % "test",
  "org.scala-lang.modules"  %% "scala-parser-combinators" % "1.0.4",
  "org.stanch"              %% "reftree"                  % "0.8.2",
  "com.propensive"          % "rapture-io" % "0.7.2"
)
