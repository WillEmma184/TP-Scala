val scala3Version = "3.3.7"

lazy val root = project
  .in(file("."))
  .settings(
    name := "fp-scala-seance-8-etl",
    version := "2.0.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.6",
      "io.circe" %% "circe-generic" % "0.14.6",
      "io.circe" %% "circe-parser" % "0.14.6",
      "org.scalameta" %% "munit" % "1.0.0" % Test
    )
  )
