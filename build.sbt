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
  "com.propensive"          % "rapture-io"                % "0.7.2",
  "com.typesafe.akka"       %% "akka-http"                % "10.0.4",
  "com.typesafe.akka"       %% "akka-http-spray-json"     % "10.0.4",
  "com.typesafe.slick"      %% "slick"                    % "3.2.0",
  "org.slf4j"               % "slf4j-nop"                 % "1.6.4",
  "com.typesafe.slick"      %% "slick-hikaricp"           % "3.2.0",
  "org.postgresql"          % "postgresql"                % "9.4.1212",
  "org.scalaz"              %% "scalaz-core"              % "7.2.10"
)
