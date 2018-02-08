name := "tipsy"

version := "0.1"

scalaVersion := "2.12.4"

resolvers ++= Seq(
  Resolver.bintrayRepo("stanch", "maven"),
  Resolver.bintrayRepo("drdozer", "maven"),
  Resolver.bintrayRepo("hseeberger", "maven"),
  Resolver.bintrayRepo("Typesafe Releases",
    "http://repo.typesafe.com/typesafe/releases"),
  Resolver.bintrayRepo("Sonatype Releases", "https://oss.sonatype.org/content/repositories/releases/")
)

// scalacOptions += "-Ywarn-unused-import"

libraryDependencies ++= Seq(
  "org.scala-lang.modules"  %% "scala-parser-combinators" % "1.0.4",
  "com.propensive"          % "rapture-io"                % "0.7.2",
  "com.typesafe.akka"       %% "akka-http"                % "10.1.0-RC1",
  "com.typesafe.slick"      %% "slick"                    % "3.2.0",
  "com.typesafe.slick"      %% "slick-hikaricp"           % "3.2.0",
  "org.postgresql"          % "postgresql"                % "42.0.0",
  "org.scalaz"              %% "scalaz-core"              % "7.2.10",
  "com.github.tminglei"     %% "slick-pg"                 % "0.15.7",
  "com.github.tminglei"     %% "slick-pg_circe-json"      % "0.15.7",
  "com.typesafe.akka"       %% "akka-actor"               % "2.5.8",
  "org.scalanlp"            %% "breeze"                   % "0.13.1",
  "de.heikoseeberger"       %% "akka-http-circe"          % "1.20.0-RC1",
  "org.slf4j"               % "slf4j-api"                 % "1.7.25",
  "org.slf4j"               % "slf4j-simple"              % "1.7.25",
  "org.scala-lang"          % "scala-compiler"            % "2.8.1",
  "com.github.scopt"        %% "scopt"                    % "3.7.0"
)

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % "0.9.1")


addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

scalafixSettings
sbtfixSettings
scalacOptions += "-Ywarn-unused-import"
